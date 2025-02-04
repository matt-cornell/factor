use crate::settings::ClientSettings;
use bevy::prelude::*;
use factor_common::coords::get_dist;
use factor_common::data::{DefaultPlayer, Position};
use factor_common::{healpix, PLANET_RADIUS};

/// The server and client need to have separate chunks, this component marks the client's
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Component)]
pub struct ClientChunk;

#[derive(Debug, Default, Clone, Resource)]
pub struct ChunkInterest {
    pub chunks: Vec<u64>,
}
impl ChunkInterest {
    pub const fn new() -> Self {
        Self { chunks: Vec::new() }
    }
}

#[derive(Debug, Default, Clone, Event)]
pub struct InterestChanged;

pub fn update_interest(
    mut commands: Commands,
    mut interest: ResMut<ChunkInterest>,
    player: Single<Ref<Position>, DefaultPlayer>,
    settings: Res<ClientSettings>,
) {
    if !(settings.is_changed() || player.is_changed() || interest.is_changed()) {
        return;
    }
    let abs = player.get_absolute().normalized();
    let old = std::mem::replace(
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
    interest.chunks.sort_by_cached_key(|c| {
        ordered_float::OrderedFloat(get_dist(abs, healpix::Layer::new(16).center(*c)))
    });
    if interest.chunks != old {
        commands.trigger(InterestChanged);
    }
}
