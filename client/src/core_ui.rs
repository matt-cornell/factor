use bevy::prelude::*;
use bevy_egui::{egui, EguiContexts};
use std::ops::Deref;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, States)]
pub enum ClientState {
    /// Main menu. This is where we start.
    #[default]
    MainMenu,
    /// Singleplayer selection.
    ///
    /// This crate doesn't have any special handling for it, as that's expected to be handled in the combined crate. Still, we need this so we can transition to this state.
    SPSelect,
    /// Multiplayer selection screen.
    MPSelect,
    /// Settings screen.
    Settings,
    /// A paused game.
    ///
    /// Whether or not the world is still running depends on the server implementation, but that's not our problem.
    Paused,
    /// The world is loading.
    WorldLoading,
    /// Actually in the game now, showing the world.
    Running,
    /// Some other state to be handled by someone else.
    ///
    /// The string is a marker that can be used for other, more advanced states to determine if their state is active.
    Other(&'static str),
}

/// Stores the last state for the `ClientState`.
///
/// This is useful for "back" buttons.
#[derive(Debug, PartialEq, Eq, Hash, Resource)]
pub struct LastState(pub(crate) ClientState);
impl Deref for LastState {
    type Target = ClientState;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub fn track_state_changes(state: Res<State<ClientState>>, mut last_state: ResMut<LastState>) {
    last_state.0 = **state;
}

pub fn render_main_menu(
    mut contexts: EguiContexts,
    mut next_state: ResMut<NextState<ClientState>>,
    config: Res<crate::ClientPlugin>,
) {
    egui::Area::new(egui::Id::new("Main Menu"))
        .anchor(egui::Align2::CENTER_CENTER, egui::Vec2::ZERO)
        .show(contexts.ctx_mut(), |ui| {
            ui.heading("Factor");
            if ui
                .add_enabled(
                    config.can_use_singleplayer,
                    egui::Button::new("Singleplayer"),
                )
                .on_disabled_hover_text(
                    "Singleplayer worlds are not supported here! Try using a different build.",
                )
                .clicked()
            {
                next_state.set(ClientState::SPSelect);
            }
            if ui
                .add_enabled(false, egui::Button::new("Multiplayer"))
                .on_disabled_hover_text("Multiplayer is planned, but not implemented yet :(")
                .clicked()
            {
                next_state.set(ClientState::MPSelect);
            }
            if ui.button("Settings").clicked() {
                next_state.set(ClientState::Settings);
            }
        });
}

pub fn render_settings(
    mut contexts: EguiContexts,
    last_state: Res<LastState>,
    mut next_state: ResMut<NextState<ClientState>>,
) {
    egui::Area::new(egui::Id::new("Settings"))
        .anchor(egui::Align2::CENTER_CENTER, egui::Vec2::ZERO)
        .show(contexts.ctx_mut(), |ui| {
            ui.heading("Settings");

            ui.label("No settings here yet, check back later!");

            if ui.button("Back").clicked() {
                next_state.set(**last_state);
            }
        });
}
