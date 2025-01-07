use crate::coords::*;
use crate::healpix;
use crate::healpix::cds::compass_point::MainWind;
use bevy::math::Affine2;
use bevy::prelude::*;
use std::convert::Infallible;
use std::sync::{LazyLock, OnceLock};
use triomphe::Arc;

pub const SCALE: f64 = 100000.0;

pub const NAN_TRANSFORM: Transform = Transform {
    translation: Vec3::NAN,
    rotation: Quat::NAN,
    scale: Vec3::NAN,
};

static CACHE: [LazyLock<
    quick_cache::sync::Cache<u64, ([Vec2; 4], Arc<OnceLock<[(u64, Transform); 8]>>)>,
>; 29] = [const { LazyLock::new(|| quick_cache::sync::Cache::new(65536)) }; 29];

fn cache_data(depth: u8, hash: u64) -> ([Vec2; 4], Arc<OnceLock<[(u64, Transform); 8]>>) {
    let Ok(res) = CACHE[depth as usize].get_or_insert_with(&hash, || {
        let layer = healpix::Layer::new(depth);
        let vertices = layer.vertices(hash);
        let center = layer.center(hash);
        let corners = vertices.map(|c2| (get_relative(center, c2) * SCALE).as_vec2()); // I don't know where this 2 came from
        Ok::<_, Infallible>((corners, Arc::new(OnceLock::new())))
    });
    res
}
pub fn corners_of(depth: u8, hash: u64) -> [Vec2; 4] {
    cache_data(depth, hash).0
}
pub fn transforms_for(depth: u8, base: u64, neighbor: u64) -> Transform {
    if base == neighbor {
        return Transform::IDENTITY;
    }
    let (verts, slice) = cache_data(depth, base); // verts: S E N W
    let transforms = slice.get_or_init(|| {
        let _guard = info_span!("finding transforms", depth, base).entered();
        let layer = healpix::Layer::new(depth);
        let neighbors = layer.neighbors(base, false);
        let mut out_verts = [[Vec2::NAN; 4]; 8]; // NW NE SE SW N E S W
        let mut out = [(u64::MAX, NAN_TRANSFORM); 8]; // same order
        for (i, wind) in [MainWind::NW, MainWind::NE, MainWind::SE, MainWind::SW]
            .into_iter()
            .enumerate()
        {
            let cell = *neighbors.get(wind).unwrap();
            let _guard = info_span!("ordinal transform", wind = ?[MainWind::NW, MainWind::NE, MainWind::SE, MainWind::SW][i], neighbor = cell).entered();
            let mut new_verts = corners_of(depth, cell); // S E N W
            // new_verts.rotate_left(1);
            let [na, nb, ba, bb] = match i {
                0 => [0, 1, 3, 2],
                1 => [3, 0, 2, 1],
                2 => [2, 3, 1, 0],
                3 => [1, 2, 0, 3],
                _ => unreachable!(),
            };
            let (v1, v2, nv1, nv2) = (verts[ba], verts[bb], new_verts[na], new_verts[nb]);
            let ax1 = nv1 - nv2;
            let ax2 = v1 - v2;
            debug!(?verts, ?new_verts, "available points");
            debug!(%v1, %v2, %nv1, %nv2, "got points");
            debug!(%ax1, %ax2, "created axes");
            let scale = (ax2.length_squared() / ax1.length_squared()).sqrt();
            debug!(scale, "found scale");
            out_verts[i][na] = verts[ba];
            out_verts[i][nb] = verts[bb];
            for v in &mut new_verts {
                *v /= scale;
            }
            let angle = ax2.to_angle() - ax1.to_angle();
            debug!(angle, "found angle");
            let mat2 = Mat2::from_scale_angle(Vec2::splat(scale), angle);
            let transformed = mat2.mul_mat2(&Mat2::from_cols(nv1, nv2));
            let mv1 = transformed.x_axis;
            let translate = v1 - mv1;
            #[cfg(debug_assertions)]
            {
                let mv2 = transformed.y_axis;
                let trans2 = v2 - mv2;
                let len = (translate - trans2).length();
                assert!(
                    len < 0.01,
                    "translations are significantly different: {} vs {}",
                    translate,
                    trans2
                );
            }
            debug!(%translate, "found translation");
            let nc = (na + 2) % 4;
            let nd = (nb + 2) % 4;
            let transformed = mat2.mul_mat2(&Mat2::from_cols(new_verts[nc], new_verts[nd]))
                + Mat2::from_cols(translate, translate);
            out_verts[i][nc] = transformed.x_axis;
            out_verts[i][nd] = transformed.y_axis;
            // out_verts[i].rotate_right(1);
            out[i] = (
                cell,
                Transform {
                    translation: translate.extend(0.0).xzy(),
                    rotation: Quat::from_rotation_y(-angle),
                    scale: Vec3::new(scale, 1.0, scale),
                },
            );
        }
        debug!(verts = ?out_verts[..4], "initial verticces");
        for (i, wind) in [MainWind::N, MainWind::E, MainWind::S, MainWind::W]
            .into_iter()
            .enumerate()
        {
            let Some(&cell) = neighbors.get(wind) else {
                continue;
            };
            let _guard = info_span!("cardinal transform", wind = ?[MainWind::N, MainWind::E, MainWind::S, MainWind::W][i], neighbor = cell).entered();
            let prev = i % 4;
            let next = (i + 1) % 4;
            let new_verts = corners_of(depth, cell); // S E N W
            let [other_v, to_base_v, to_prev_v, to_next_v] = match i {
                0 => [2, 0, 3, 1],
                1 => [1, 3, 2, 0],
                2 => [0, 2, 1, 3],
                3 => [3, 1, 0, 2],
                _ => unreachable!(),
            };
            let to = Mat3::from_cols(
                verts[other_v].extend(1.0),
                out_verts[prev][other_v].extend(1.0),
                out_verts[next][other_v].extend(1.0),
            );
            let from = Mat3::from_cols(
                new_verts[to_base_v].extend(1.0),
                new_verts[to_prev_v].extend(1.0),
                new_verts[to_next_v].extend(1.0),
            );
            debug!(%from, %to, "computing transform");
            let mat3 = to.mul_mat3(&from.inverse());
            #[cfg(debug_assertions)]
            {
                let bottom = mat3.transpose().z_axis;
                assert!((bottom - Vec3::Z).length() < 0.001, "transform isn't affine, bottom row = {bottom}");
            }
            debug!(mat = %mat3, "found transformation");
            let (scale, angle, trans) = Affine2::from_mat3(mat3).to_scale_angle_translation();
            debug!(%scale, angle, %trans, "decomposed matrix");
            out[i + 4] = (
                cell,
                Transform {
                    translation: trans.extend(0.0).xzy(),
                    rotation: Quat::from_rotation_y(-angle),
                    scale: scale.extend(1.0).xzy(),
                },
            );
        }
        out
    });
    transforms
        .iter()
        .find_map(|&(n, t)| (n == neighbor).then_some(t))
        .unwrap_or_else(|| {
            error!(
                depth,
                base, neighbor, "attempted to find transform for non-neighboring cell"
            );
            NAN_TRANSFORM
        })
}
