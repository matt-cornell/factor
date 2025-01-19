use bevy::ecs::system::SystemChangeTick;
use bevy::prelude::*;
use bevy::utils::HashMap;
use factor_client::chunks::{ClientChunk, InterestChanged as ClientInterestChanged};
use factor_client::core_ui::ClientState;
use factor_common::cell::transforms_for;
use factor_common::data::{ChunkId, ChunkInterest, DefaultPlayer, PlayerId, Position};
use factor_common::mesh::MeshData;
use factor_server::chunk::{InterestChanged as ServerInterestChanged, ServerChunk};
use factor_server::player::{PlayerLoaded, PlayerRequest};
use factor_server::terrain::bevy::TerrainReady;
use factor_server::utils::database::Database;
use factor_server::ServerState;
use std::error::Error;
use bevy::pbr::wireframe::WireframeConfig;
use unsize::*;
use factor_client::settings::DebugSettings;

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
    world.remove_resource::<DebugSettings>();
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
        commands.add_observer(player_loaded).insert(SPListener);
        commands.add_observer(interest_changed).insert(SPListener);
        commands.init_resource::<DebugSettings>();
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
        next_state.set(ClientState::Paused);
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

pub fn surface_mesh(
    tick: SystemChangeTick,
    mut commands: Commands,
    pos: Single<&Position, DefaultPlayer>,
    player_interest: Single<&ChunkInterest, DefaultPlayer>,
    mut client_chunks: Query<
        (Entity, &ChunkId, Option<Ref<MeshData>>),
        (With<ClientChunk>, Without<ServerChunk>),
    >,
    mut server_chunks: Query<(&ChunkId, Ref<MeshData>), (With<ServerChunk>, Without<ClientChunk>)>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    let mut client_lookup = client_chunks
        .iter_mut()
        .map(|e| (e.1 .0, (e.0, e.2)))
        .collect::<HashMap<_, _>>();
    for (ChunkId(id), mesh) in server_chunks.iter_mut() {
        if let Some((entity, cmesh)) = client_lookup.remove(id) {
            if cmesh.is_none_or(|cmesh| {
                mesh.last_changed()
                    .is_newer_than(cmesh.last_changed(), tick.this_run())
            }) {
                commands
                    .entity(entity)
                    .insert((mesh.clone(), Mesh3d(meshes.add(mesh.clone().build_bevy()))));
            }
        } else if player_interest.chunks.contains(*id) {
            commands.spawn((
                ChunkId(*id),
                mesh.clone(),
                Mesh3d(meshes.add(mesh.clone().build_bevy())),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::BLACK,
                    cull_mode: None,
                    ..default()
                })),
                transforms_for(12, pos.frame, *id >> 8),
                ClientChunk,
            ));
        }
    }
}
