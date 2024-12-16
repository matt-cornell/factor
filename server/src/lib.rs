#![feature(array_chunks, iter_array_chunks, path_add_extension, try_blocks)]
use bevy::ecs::system::SystemId;
use bevy::prelude::*;
use bevy::time::common_conditions::on_timer;
use orbit::OrbitPlugin;
use terrain::bevy::*;

pub mod config;
pub mod orbit;
pub mod player;
pub mod storage;
pub mod tables;
pub mod terrain;
pub mod utils;

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
            .add_sub_state::<SetupTectonics>()
            .add_sub_state::<RunningTectonics>()
            .add_sub_state::<RunningClimate>()
            .add_event::<player::PlayerRequest>()
            .add_event::<player::PlayerLoaded>()
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
            .add_observer(player::load_player);
    }
}
