use bevy::prelude::*;
use bevy_egui::{egui, EguiContexts};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, States)]
pub enum BaseClientState {
    #[default]
    MainMenu,
    RunningSP,
    RunningMP,
}
impl From<BaseClientState> for ClientState {
    fn from(value: BaseClientState) -> Self {
        match value {
            BaseClientState::MainMenu => ClientState::MainMenu,
            BaseClientState::RunningSP => ClientState::RunningSP,
            BaseClientState::RunningMP => ClientState::RunningMP,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, States)]
pub enum ClientState {
    #[default]
    MainMenu,
    SPSelect,
    MPSelect,
    Settings(BaseClientState),
    Paused(BaseClientState),
    RunningSP,
    RunningMP,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, SubStates)]
#[source(ClientState = ClientState::RunningSP | ClientState::RunningMP)]
pub struct RunningGame;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, SubStates)]
#[source(ClientState = ClientState::Paused(_))]
pub struct Paused;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, SubStates)]
#[source(ClientState = ClientState::Settings(_))]
pub struct InSettings;

pub fn render_main_menu(
    mut contexts: EguiContexts,
    mut state: ResMut<NextState<ClientState>>,
    config: Res<super::ClientPlugin>,
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
                state.set(ClientState::SPSelect);
            }
            if ui
                .add_enabled(false, egui::Button::new("Multiplayer"))
                .on_disabled_hover_text("Multiplayer is planned, but not implemented yet :(")
                .clicked()
            {
                state.set(ClientState::MPSelect);
            }
            if ui.button("Settings").clicked() {
                state.set(ClientState::Settings(BaseClientState::MainMenu));
            }
        });
}
