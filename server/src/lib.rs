#![feature(iter_array_chunks, path_add_extension)]
use bevy::prelude::*;

pub mod config;
pub mod orbit;
pub mod storage;
pub mod terrain;
pub mod utils;

pub struct ServerPlugin;
impl Plugin for ServerPlugin {
    fn build(&self, app: &mut App) {
        app.init_asset_loader::<config::WorldConfigLoader>();
    }
}
