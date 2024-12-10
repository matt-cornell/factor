use crate::player::PlayerPos;
use crate::settings::ClientSettings;
use crate::utils::{mesh_quad, MeshPoint};
use bevy::prelude::*;
use factor_common::cell::{corners_of, transforms_for};
use factor_common::coords::get_absolute;
use factor_common::{healpix, PLANET_RADIUS};
use quick_cache::sync::Cache;
use std::ops::{Add, Mul};

#[derive(Debug, Clone, Component)]
pub struct Chunk {
    /// HEALPix hash for this chunk, at depth 16
    pub hash: u64,
    /// SENW ordering, as always
    pub corner_heights: [f32; 4],
}

/// The current state of the chunk
#[derive(Debug, Clone, PartialEq, PartialOrd, Component)]
pub enum ChunkState {
    Uninit,
    NeedsUpdate,
    Loaded,
}

#[derive(Debug, Default, Resource)]
pub struct HighOctaveNoise {
    pub layers: Vec<HighOctaveNoiseLayer>,
}

#[derive(Debug)]
pub struct HighOctaveNoiseLayer {
    pub depth: u8,
    pub shift: f32,
    pub cache: LayerCache,
}

#[derive(Debug)]
pub enum LayerCache {
    Value(Cache<u64, f32>),
    Gradient(Cache<u64, Vec2>),
}

#[derive(Debug, Clone, Resource)]
pub struct ChunkInterest {
    pub chunks: tinyset::SetU64,
}
impl ChunkInterest {
    pub fn new() -> Self {
        Self {
            chunks: tinyset::SetU64::new(),
        }
    }
}
impl Default for ChunkInterest {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Event)]
pub struct ReloadTerrain {
    pub chunk: u64,
}
impl ReloadTerrain {
    pub const fn new(chunk: u64) -> Self {
        Self { chunk }
    }
}

pub fn update_interest(
    mut interest: ResMut<ChunkInterest>,
    player: Res<PlayerPos>,
    settings: Res<ClientSettings>,
) {
    let layer = healpix::Layer::new(16);
    let abs = get_absolute(
        layer.center(player.chunk),
        player.position.xz().as_dvec2() / PLANET_RADIUS,
    )
    .normalized();
    if settings.is_changed() || player.is_changed() {
        interest.chunks = healpix::nested::cone_coverage_approx(
            16,
            abs.lon,
            abs.lat,
            settings.render_distance / PLANET_RADIUS,
        )
        .flat_iter()
        .collect();
    }
}
pub fn render_chunks(
    mut commands: Commands,
    interest: Res<ChunkInterest>,
    mut query: Query<(Entity, &Chunk, &mut ChunkState)>,
    assets: Res<AssetServer>,
    mut reloads: EventReader<ReloadTerrain>,
    // noise: Res<HighOctaveNoise>,
) {
    use factor_common::healpix::nested::zordercurve::*;
    let zoc = get_zoc(4);
    let mut reloaded = tinyset::SetU64::new();
    reloaded.extend(reloads.read().map(|r| r.chunk));
    for (id, chunk, mut state) in query.iter_mut() {
        if interest.chunks.contains(chunk.hash) {
            if *state < ChunkState::Loaded || reloaded.contains(chunk.hash) {
                *state = ChunkState::Loaded;
                let base_hash = chunk.hash >> 8;
                let base_corners = corners_of(12, base_hash); // SENW
                let transform = transforms_for(12, base_hash, chunk.hash >> 8);
                let ij = zoc.h2ij(chunk.hash & 0xff);
                let i = zoc.ij2i(ij);
                let j = zoc.ij2j(ij);
                let chunk_corners = std::array::from_fn(|n| {
                    let (i, j) = match n {
                        0 => (i, j),
                        1 => (i + 1, j),
                        2 => (i + 1, j + 1),
                        3 => (i, j + 1),
                        _ => unreachable!(),
                    };
                    let i = i as f32 * 0.5;
                    let j = j as f32 * 0.5;
                    bilinear(i, j, base_corners)
                });
                let mesh = mesh_quad(chunk_corners, 32, |MeshPoint { abs: _abs, i, j }| {
                    bilinear(i as f32 / 32.0, j as f32 / 32.0, chunk.corner_heights)
                    // TODO: high octave noise
                });
                commands.entity(id).insert(PbrBundle {
                    mesh: assets.add(mesh),
                    transform,
                    ..default()
                });
            }
        } else {
            commands.entity(id).despawn_recursive();
        }
    }
}
fn bilinear<T: Mul<f32, Output = T> + Add<Output = T>>(i: f32, j: f32, verts: [T; 4]) -> T {
    let [s, e, n, w] = verts;
    let se = e * i + s * (1.0 - i);
    let nw = n * i + w * (1.0 - i);
    nw * j + se * (1.0 - j)
}
