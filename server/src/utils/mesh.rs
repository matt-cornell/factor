use bevy::math::{Vec2, Vec3, Vec3Swizzles};
use bevy::utils::ConditionalSend;
use std::cmp::Ordering;
use bevy::tasks::futures_lite::future::yield_now;
pub use factor_common::mesh::MeshData;

/// A point to be given to the height generator function. Gives both absolute position and i,j coordinates.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MeshPoint {
    pub abs: Vec2,
    pub i: usize,
    pub j: usize,
}

pub fn try_mesh_quad<E>(
    corners: [Vec2; 4],
    depth: usize,
    mut height: impl FnMut(MeshPoint) -> Result<f32, E>,
) -> Result<MeshData, E> {
    let len = depth + 1;
    let nverts = len * len;
    let (min_x, max_x, min_y, max_y) = corners.iter().fold(
        (
            f32::INFINITY,
            f32::NEG_INFINITY,
            f32::INFINITY,
            f32::NEG_INFINITY,
        ),
        |(ix, ax, iy, ay), &Vec2 { x, y }| (ix.min(x), ax.max(x), iy.min(y), ay.max(y)),
    );
    let scale_x = (max_x - min_x).recip();
    let scale_y = (max_y - min_y).recip();
    let uv_add = Vec2::new(-min_x, -min_y);
    let uv_scale = Vec2::new(scale_x, scale_y);
    let mut vertices = vec![Vec3::NAN; nverts];
    let mut uv = vec![Vec2::NAN; nverts];
    let mut triangles = Vec::with_capacity(2 * nverts);
    let [v0, v1, v2, v3] = corners;
    let va = v1 - v0;
    let vb = v2 - v0;
    let vc = v3 - v0;
    let scale = (depth as f32).recip();
    for i in 0..len {
        for j in 0..len {
            let idx = i * len + j;
            let basis = match i.cmp(&j) {
                Ordering::Less => {
                    triangles.extend_from_slice(&[
                        [idx as u32, (idx - 1) as u32, (idx + len) as u32],
                        [(idx - 1) as u32, (idx + len - 1) as u32, (idx + len) as u32],
                    ]);
                    va * scale * i.abs_diff(j) as f32
                }
                Ordering::Greater => {
                    triangles.extend_from_slice(&[
                        [idx as u32, (idx + 1) as u32, (idx - len) as u32],
                        [(idx + 1) as u32, (idx - len + 1) as u32, (idx - len) as u32],
                    ]);
                    vc * scale * i.abs_diff(j) as f32
                }
                Ordering::Equal => Vec2::ZERO,
            };
            let b_basis = vb * scale * i.min(j) as f32;
            let vert = b_basis + basis + v0;
            vertices[idx] = vert.extend(height(MeshPoint { abs: vert, i, j })?).xzy();
            uv[idx] = (vert + uv_add) * uv_scale;
        }
    }
    Ok(MeshData {
        vertices,
        triangles,
        uv,
    })
}

/// Same as `try_mesh_quad` but it's async and breaks after each chunk. It doesn't actually have to wait at all, this is just so it's fairer when running on a thread pool.
pub async fn async_try_mesh_quad<E>(
    corners: [Vec2; 4],
    depth: usize,
    mut height: impl FnMut(MeshPoint) -> Result<f32, E> + ConditionalSend,
) -> Result<MeshData, E> {
    let len = depth + 1;
    let nverts = len * len;
    let (min_x, max_x, min_y, max_y) = corners.iter().fold(
        (
            f32::INFINITY,
            f32::NEG_INFINITY,
            f32::INFINITY,
            f32::NEG_INFINITY,
        ),
        |(ix, ax, iy, ay), &Vec2 { x, y }| (ix.min(x), ax.max(x), iy.min(y), ay.max(y)),
    );
    let scale_x = (max_x - min_x).recip();
    let scale_y = (max_y - min_y).recip();
    let uv_add = Vec2::new(-min_x, -min_y);
    let uv_scale = Vec2::new(scale_x, scale_y);
    let mut vertices = vec![Vec3::NAN; nverts];
    let mut uv = vec![Vec2::NAN; nverts];
    let mut triangles = Vec::with_capacity(2 * nverts);
    let [v0, v1, v2, v3] = corners;
    let va = v1 - v0;
    let vb = v2 - v0;
    let vc = v3 - v0;
    let scale = (depth as f32).recip();
    for i in 0..len {
        for j in 0..len {
            let idx = i * len + j;
            let basis = match i.cmp(&j) {
                Ordering::Less => {
                    triangles.extend_from_slice(&[
                        [idx as u32, (idx - 1) as u32, (idx + len) as u32],
                        [(idx - 1) as u32, (idx + len - 1) as u32, (idx + len) as u32],
                    ]);
                    va * scale * i.abs_diff(j) as f32
                }
                Ordering::Greater => {
                    triangles.extend_from_slice(&[
                        [idx as u32, (idx + 1) as u32, (idx - len) as u32],
                        [(idx + 1) as u32, (idx - len + 1) as u32, (idx - len) as u32],
                    ]);
                    vc * scale * i.abs_diff(j) as f32
                }
                Ordering::Equal => Vec2::ZERO,
            };
            let b_basis = vb * scale * i.min(j) as f32;
            let vert = b_basis + basis + v0;
            vertices[idx] = vert.extend(height(MeshPoint { abs: vert, i, j })?).xzy();
            yield_now().await; // add in breaks
            uv[idx] = (vert + uv_add) * uv_scale;
        }
    }
    Ok(MeshData {
        vertices,
        triangles,
        uv,
    })
}
