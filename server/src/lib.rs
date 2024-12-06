#![feature(iter_array_chunks, path_add_extension)]
use bevy::ecs::system::SystemId;
use bevy::prelude::*;

pub mod config;
pub mod orbit;
pub mod player;
pub mod state;
pub mod storage;
pub mod tables;
pub mod terrain;
pub mod utils;

#[derive(Debug, Clone, Copy, Resource)]
pub struct ServerSystems {
    pub new_player: SystemId<(), player::PlayerDataExt>,
}

pub struct ServerPlugin;
impl Plugin for ServerPlugin {
    fn build(&self, app: &mut App) {
        let new_player = app.register_system(player::new_player);
        app.init_asset_loader::<config::WorldConfigLoader>()
            .insert_resource(ServerSystems { new_player });
    }
}
