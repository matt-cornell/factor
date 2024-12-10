#![feature(try_blocks)]
use bevy::ecs::system::SystemId;
use bevy::prelude::*;
use factor_client::core_ui::ClientState;
use factor_server::utils::database as redb;
use state::*;

pub mod glue;
pub mod render;
pub mod state;

#[derive(Debug, Clone, Copy, Resource)]
pub struct ComboSystems {
    pub after_loaded: SystemId<Result<(), redb::Error>>,
}

#[derive(Debug, Clone, Copy)]
pub struct CombinedPlugin;
impl Plugin for CombinedPlugin {
    fn build(&self, app: &mut App) {
        let after_loaded = app.register_system(glue::after_loaded);
        app.insert_state(SingleplayerState::Base(ClientState::MainMenu))
            .add_sub_state::<CreatingWorld>()
            .insert_resource(ComboSystems { after_loaded })
            .add_systems(PreUpdate, link_states)
            .add_systems(
                Update,
                (
                    render::render_select_sp.run_if(in_state(ClientState::SPSelect)),
                    render::render_create_world.run_if(in_state(CreatingWorld)),
                    render::render_creating_world
                        .run_if(in_state(SingleplayerState::CreatingWorld)),
                ),
            );
    }
}
