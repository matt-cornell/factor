#![feature(try_blocks)]
use bevy::prelude::*;
use factor_client::core_ui::ClientState;
use factor_server::ServerState;
use state::*;

pub mod glue;
pub mod render;
pub mod state;

#[derive(Debug, Clone, Copy)]
pub struct CombinedPlugin;
impl Plugin for CombinedPlugin {
    fn build(&self, app: &mut App) {
        app.insert_state(SingleplayerState::Base(ClientState::MainMenu))
            .add_sub_state::<CreatingWorld>()
            .add_systems(PreUpdate, link_states)
            .add_systems(
                Update,
                (
                    render::render_select_sp.run_if(in_state(ClientState::SPSelect)),
                    render::render_create_world.run_if(in_state(CreatingWorld)),
                    render::render_creating_world
                        .run_if(in_state(SingleplayerState::CreatingWorld)),
                ),
            )
            .add_systems(
                OnExit(factor_client::core_ui::WorldLoaded(true)),
                |mut next_state: ResMut<NextState<ServerState>>| {
                    next_state.set(ServerState::Disabled);
                },
            )
            .add_systems(
                OnEnter(factor_server::ServerState::Disabled),
                glue::cleanup_server,
            )
            .add_observer(glue::after_loaded);
    }
}
