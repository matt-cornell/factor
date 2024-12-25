use crate::settings::ClientSettings;
use bevy::prelude::*;
use factor_common::coords::get_absolute;
use factor_common::data::{ChunkInterest, DefaultPlayer, Position};
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

#[derive(Debug, Clone, Copy, PartialEq, Event)]
pub struct ReloadTerrain {
    pub chunk: u64,
}
impl ReloadTerrain {
    pub const fn new(chunk: u64) -> Self {
        Self { chunk }
    }
}

#[derive(Debug, Default, Clone, Event)]
pub struct InterestChanged {
    pub added: tinyset::SetU64,
    pub removed: tinyset::SetU64,
}

pub fn update_interest(
    mut commands: Commands,
    mut interest: Single<&mut ChunkInterest, DefaultPlayer>,
    player: Single<Ref<Position>, DefaultPlayer>,
    settings: Res<ClientSettings>,
) {
    if !(settings.is_changed() || player.is_changed() || interest.is_changed()) {
        return;
    }
    println!("update");
    let layer = healpix::Layer::new(16);
    let abs = get_absolute(
        layer.center(player.chunk),
        player.pos.xz().as_dvec2() / PLANET_RADIUS,
    )
    .normalized();
    let mut old = std::mem::replace(
        &mut interest.chunks,
        healpix::nested::cone_coverage_approx(
            16,
            abs.lon,
            abs.lat,
            settings.render_distance / PLANET_RADIUS,
        )
        .flat_iter()
        .collect(),
    );
    let mut new = interest.chunks.clone();
    let mut intersection = tinyset::SetU64::new();
    if old.len() > new.len() {
        for i in new.iter() {
            if old.remove(i) {
                intersection.insert(i);
            }
        }
        for i in intersection {
            new.remove(i);
        }
    } else {
        for i in old.iter() {
            if new.remove(i) {
                intersection.insert(i);
            }
        }
        for i in intersection {
            old.remove(i);
        }
    }
    if !(old.is_empty() && new.is_empty()) {
        debug!(added = new.len(), removed = old.len(), "interest changed");
        commands.trigger(InterestChanged {
            added: new,
            removed: old,
        });
    }
}
