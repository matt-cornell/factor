use bevy::prelude::*;
use factor_client::chunks::InterestChanged as ClientInterestChanged;
use factor_client::core_ui::ClientState;
use factor_common::data::PlayerId;
use factor_server::chunk::InterestChanged as ServerInterestChanged;
use factor_server::player::{PlayerLoaded, PlayerRequest};
use factor_server::terrain::bevy::TerrainReady;
use factor_server::utils::database::Database;
use factor_server::ServerState;
use std::error::Error;
use unsize::*;

/// Marker component for singleplayer event listeners that we should despawn when we exit singleplayer
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Component)]
pub struct SPListener;

/// Cleanup all entities with the [`SPListener`] component
pub fn cleanup_server(world: &mut World) {
    let to_despawn = world
        .query_filtered::<Entity, With<SPListener>>()
        .iter(world)
        .collect::<Vec<_>>();
    for entity in to_despawn {
        world.despawn(entity);
    }
    world.remove_resource::<Database>();
}

pub fn after_loaded(
    mut trig: Trigger<TerrainReady>,
    mut commands: Commands,
    mut next_state: ResMut<NextState<ClientState>>,
    mut next_server: ResMut<NextState<ServerState>>,
) {
    if let Err(err) = std::mem::replace(&mut trig.res, Ok(())) {
        next_state.set(ClientState::LoadingFailed {
            cause: triomphe::Arc::new(err).unsize(Coercion!(to dyn Error + Send + Sync)),
            return_to_sp: true,
        });
    } else {
        commands.add_observer(player_loaded);
        commands.add_observer(interest_changed).insert(SPListener);
        commands.trigger(PlayerRequest(PlayerId::DEFAULT));
        next_state.set(ClientState::WorldLoading);
        next_server.set(ServerState::Running);
    }
}

pub fn player_loaded(
    trigger: Trigger<PlayerLoaded>,
    mut commands: Commands,
    mut next_state: ResMut<NextState<ClientState>>,
) {
    debug_assert_eq!(trigger.event().id, PlayerId::DEFAULT);
    commands.entity(trigger.observer()).despawn();
    if let Err(err) = &trigger.event().res {
        next_state.set(ClientState::LoadingFailed {
            cause: err.clone(),
            return_to_sp: true,
        });
    } else {
        next_state.set(ClientState::Running);
    }
}

pub fn interest_changed(
    mut trig: Trigger<ClientInterestChanged>,
    mut events: EventWriter<ServerInterestChanged>,
) {
    let ClientInterestChanged { added, removed } = std::mem::take(trig.event_mut());
    events.send(ServerInterestChanged {
        player: PlayerId::DEFAULT,
        needs_update: false,
        added,
        removed,
    });
}
