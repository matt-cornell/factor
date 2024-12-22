use crate::player::PlayerPos;
use crate::settings::ClientSettings;
use bevy::prelude::*;
use factor_common::coords::get_absolute;
use factor_common::mesh::MeshData;
use factor_common::{healpix, PLANET_RADIUS};

#[derive(Debug, Clone, Component)]
pub struct Chunk {
    /// HEALPix hash for this chunk, at depth 16
    pub hash: u64,
    /// SENW ordering, as always
    pub corner_heights: [f32; 4],
    /// The surface of the chunk
    pub surface: MeshData,
}

/// The current state of the chunk
#[derive(Debug, Clone, PartialEq, PartialOrd, Component)]
pub enum ChunkState {
    Uninit,
    NeedsUpdate,
    Loaded,
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
