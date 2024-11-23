use bevy::prelude::*;
use core_ui::*;

pub mod core_ui;

#[derive(Debug, Clone, Copy, Resource)]
pub struct ClientPlugin {
    pub can_use_singleplayer: bool,
}
impl Plugin for ClientPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(bevy_egui::EguiPlugin)
            .init_state::<ClientState>()
            .insert_resource(LastState(ClientState::MainMenu))
            .insert_resource(*self)
            .add_systems(
                Update,
                (
                    track_state_changes.run_if(state_changed::<ClientState>),
                    render_main_menu.run_if(in_state(ClientState::MainMenu)),
                    render_settings.run_if(in_state(ClientState::Settings)),
                ),
            );
    }
}
