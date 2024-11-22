use bevy::prelude::*;
use core_ui::*;

pub mod core_ui;

#[derive(Debug, Clone, Copy, Resource)]
pub struct ClientPlugin {
    can_use_singleplayer: bool,
}
impl Plugin for ClientPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(bevy_egui::EguiPlugin)
            .init_state::<ClientState>()
            .add_sub_state::<RunningGame>()
            .add_sub_state::<Paused>()
            .add_sub_state::<InSettings>()
            .insert_resource(*self)
            .add_systems(
                Update,
                render_main_menu.run_if(in_state(ClientState::MainMenu)),
            );
    }
}
