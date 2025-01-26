use bevy::math::{Vec2, Vec3, Vec3Swizzles};
pub use factor_common::mesh::MeshData;
use rayon::prelude::*;
use std::cmp::Ordering;

struct UnsafeAssertSync<T>(T);
impl<T> UnsafeAssertSync<T> {
    unsafe fn get(&self) -> &T {
        &self.0
    }
}
unsafe impl<T> Send for UnsafeAssertSync<T> {}
unsafe impl<T> Sync for UnsafeAssertSync<T> {}

/// A point to be given to the height generator function. Gives both absolute position and i,j coordinates.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MeshPoint {
    pub abs: Vec2,
    pub i: usize,
    pub j: usize,
}

pub fn try_mesh_quad<E: Send>(
    corners: [Vec2; 4],
    depth: usize,
    height: impl Fn(MeshPoint) -> Result<f32, E> + Send + Sync,
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
    let mut triangles = vec![[u32::MAX; 3]; depth * len * 2];
    let [v0, v1, v2, v3] = corners;
    let va = v1 - v0;
    let vb = v2 - v0;
    let vc = v3 - v0;
    let scale = (depth as f32).recip();
    let tri_cells =
        UnsafeAssertSync(std::cell::Cell::from_mut(triangles.as_mut_slice()).as_slice_of_cells());
    vertices
        .par_iter_mut()
        .zip(&mut uv)
        .enumerate()
        .try_for_each(|(idx, (v, uv))| {
            let tri_cells = unsafe { tri_cells.get() }; // SAFETY: we access different indices in each loop
            let i = idx / len;
            let j = idx % len;
            let basis = match i.cmp(&j) {
                Ordering::Less => {
                    tri_cells[(i * depth + j - 1) * 2].set([
                        idx as u32,
                        (idx - 1) as u32,
                        (idx + len) as u32,
                    ]);
                    tri_cells[(i * depth + j - 1) * 2 + 1].set([
                        (idx - 1) as u32,
                        (idx + len - 1) as u32,
                        (idx + len) as u32,
                    ]);
                    va * scale * i.abs_diff(j) as f32
                }
                Ordering::Greater => {
                    tri_cells[(i * depth + j) * 2].set([
                        idx as u32,
                        (idx + 1) as u32,
                        (idx - len) as u32,
                    ]);
                    tri_cells[(i * depth + j) * 2 + 1].set([
                        (idx + 1) as u32,
                        (idx - len + 1) as u32,
                        (idx - len) as u32,
                    ]);
                    vc * scale * i.abs_diff(j) as f32
                }
                Ordering::Equal => Vec2::ZERO,
            };
            let b_basis = vb * scale * i.min(j) as f32;
            let vert = b_basis + basis + v0;
            *v = vert.extend(height(MeshPoint { abs: vert, i, j })?).xzy();
            *uv = (vert + uv_add) * uv_scale;
            Ok(())
        })?;
    Ok(MeshData {
        vertices,
        triangles,
        uv,
    })
}
