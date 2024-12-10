use bevy::prelude::*;
use bevy::render::render_asset::RenderAssetUsages;
use std::cmp::Ordering;
use std::future::Future;
use std::pin::Pin;
use std::sync::OnceLock;
use std::task::{Context, Poll};

/// A point to be given to
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MeshPoint {
    pub abs: Vec2,
    pub i: usize,
    pub j: usize,
}

pub fn mesh_quad(
    corners: [Vec2; 4],
    depth: usize,
    mut height: impl FnMut(MeshPoint) -> f32,
) -> Mesh {
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
    let mut verts = vec![Vec3::NAN; nverts];
    let mut uv = vec![Vec2::NAN; nverts];
    let mut indices = Vec::with_capacity(6 * nverts);
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
                    indices.extend_from_slice(&[
                        idx as u32,
                        (idx - 1) as u32,
                        (idx + len) as u32,
                        (idx - 1) as u32,
                        (idx + len - 1) as u32,
                        (idx + len) as u32,
                    ]);
                    va * scale * i.abs_diff(j) as f32
                }
                Ordering::Greater => {
                    indices.extend_from_slice(&[
                        idx as u32,
                        (idx + 1) as u32,
                        (idx - len) as u32,
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
            verts[idx] = vert.extend(height(MeshPoint { abs: vert, i, j })).xzy();
            uv[idx] = (vert + uv_add) * uv_scale;
        }
    }
    Mesh::new(
        bevy::render::mesh::PrimitiveTopology::TriangleList,
        RenderAssetUsages::RENDER_WORLD,
    )
    .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, verts)
    .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uv)
    .with_inserted_indices(bevy::render::mesh::Indices::U32(indices))
}

#[derive(Debug)]
pub struct GetOnceLock<'a, T> {
    lock: &'a OnceLock<T>,
}
impl<T> Clone for GetOnceLock<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for GetOnceLock<'_, T> {}
impl<'a, T> GetOnceLock<'a, T> {
    pub fn new(lock: &'a OnceLock<T>) -> Self {
        Self { lock }
    }
}
impl<'a, T> Future for GetOnceLock<'a, T> {
    type Output = &'a T;
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if let Some(val) = self.lock.get() {
            Poll::Ready(val)
        } else {
            cx.waker().wake_by_ref();
            Poll::Pending
        }
    }
}
