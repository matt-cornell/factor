#![feature(
    array_chunks,
    array_try_map,
    box_as_ptr,
    iter_array_chunks,
    map_try_insert,
    path_add_extension,
    try_blocks
)]
use bevy::ecs::system::SystemId;
use bevy::prelude::*;
use bevy::time::common_conditions::on_timer;
use factor_common::data::PlayerId;
use orbit::OrbitPlugin;
use terrain::bevy::*;

pub mod chunks;
pub mod config;
pub mod orbit;
pub mod player;
pub mod storage;
pub mod tables;
pub mod terrain;
pub mod utils;

/// Current state of the server
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, States)]
pub enum ServerState {
    /// Nothing is running. When this state is entered, we're going to teardown everything we had
    #[default]
    Disabled,
    /// We aren't currently running, but we should keep everything loaded.
    Paused,
    /// We're running and doing stuff.
    Running,
}

#[derive(Debug, Clone, Copy, Resource)]
pub struct ServerSystems {
    pub setup_terrain: SystemId,
    pub load_terrain: SystemId,
}

#[derive(Debug, Default)]
pub struct ServerPlugin {
    pub orbit: OrbitPlugin,
}
impl Plugin for ServerPlugin {
    fn build(&self, app: &mut App) {
        let setup_terrain = app.register_system(setup_terrain);
        let load_terrain = app.register_system(load_terrain);
        app.init_asset_loader::<config::WorldConfigLoader>()
            .add_plugins(self.orbit)
            .init_state::<ClimatePhase>()
            .init_state::<ServerState>()
            .add_sub_state::<SetupTectonics>()
            .add_sub_state::<RunningTectonics>()
            .add_sub_state::<RunningClimate>()
            .add_event::<player::PlayerRequest>()
            .add_event::<player::PlayerLoaded>()
            .add_event::<chunks::ChunkRequest>()
            .add_event::<chunks::ChunkLoaded>()
            .add_event::<chunks::UnloadChunk>()
            .add_event::<chunks::InterestChanged>()
            .insert_resource(ClimateRunning(false))
            .insert_resource(ServerSystems {
                setup_terrain,
                load_terrain,
            })
            .add_systems(
                Update,
                (
                    update_climate.run_if(
                        resource_exists::<ClimateData>
                            .and(resource_equals(ClimateRunning(true)))
                            .and(on_timer(std::time::Duration::from_secs(5))),
                    ),
                    setup_tect.run_if(in_state(SetupTectonics)),
                    run_tect.run_if(in_state(RunningTectonics)),
                    run_climate.run_if(in_state(RunningClimate)),
                ),
            )
            .add_systems(OnEnter(ClimatePhase::NoiseSetup), setup_noise)
            .add_systems(OnEnter(ClimatePhase::ClimateSetup), setup_climate)
            .add_systems(OnEnter(ClimatePhase::Finalize), finalize)
            .add_systems(OnEnter(ServerState::Running), start_server)
            .add_systems(OnEnter(ServerState::Disabled), cleanup_server)
            .add_systems(
                Update,
                (
                    chunks::handle_interests,
                    chunks::loader_interface,
                    chunks::unload_chunks,
                    player::persist_players,
                )
                    .run_if(in_state(ServerState::Running)),
            )
            .add_observer(player::load_player)
            .add_observer(player::set_heights);
    }
}

fn start_server(
    mut commands: Commands,
    cfg: Res<config::WorldConfig>,
    db: Res<utils::database::Database>,
) {
    commands.init_resource::<chunks::LoadedChunks>();
    let handle = chunks::ChunkloaderHandle::spawn(&cfg, &db);
    commands.insert_resource(handle);
}

fn cleanup_server(world: &mut World) {
    let to_despawn = world
        .query_filtered::<Entity, With<PlayerId>>()
        .iter(world)
        .collect::<Vec<_>>();
    for entity in to_despawn {
        world.despawn(entity);
    }
    world.remove_resource::<chunks::LoadedChunks>();
    if let Some(loader) = world.remove_resource::<chunks::ChunkloaderHandle>() {
        loader.cancel();
    }
}
