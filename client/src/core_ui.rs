use crate::ClientPlugin;
use bevy::prelude::*;
use bevy_egui::{egui, EguiContexts};
use std::error::Error;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::time::Duration;
use triomphe::Arc;

#[derive(Debug, Default, Clone, States)]
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
    /// The world failed to load for some reason.
    LoadingFailed {
        /// Why the loading failed
        cause: Arc<dyn Error + Send + Sync>,
        /// If true, return to SPSelect, otherwise return to MPSelect
        return_to_sp: bool,
    },
    /// Actually in the game now, showing the world.
    Running,
    /// Some other state to be handled by someone else.
    ///
    /// The string is a marker that can be used for other, more advanced states to determine if their state is active.
    Other(&'static str),
}
impl PartialEq for ClientState {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::MainMenu, Self::MainMenu) => true,
            (Self::SPSelect, Self::SPSelect) => true,
            (Self::MPSelect, Self::MPSelect) => true,
            (Self::Settings, Self::Settings) => true,
            (Self::Paused, Self::Paused) => true,
            (Self::WorldLoading, Self::WorldLoading) => true,
            (Self::Running, Self::Running) => true,
            (
                Self::LoadingFailed {
                    cause: cause_a,
                    return_to_sp: rtsa,
                },
                Self::LoadingFailed {
                    cause: cause_b,
                    return_to_sp: rtsb,
                },
            ) => rtsa == rtsb && Arc::ptr_eq(cause_a, cause_b),
            (Self::Other(a), Self::Other(b)) => a == b,
            _ => false,
        }
    }
}
impl Eq for ClientState {}
impl Hash for ClientState {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::MainMenu => state.write_u8(0),
            Self::SPSelect => state.write_u8(1),
            Self::MPSelect => state.write_u8(2),
            Self::Settings => state.write_u8(3),
            Self::Paused => state.write_u8(4),
            Self::WorldLoading => state.write_u8(5),
            Self::Running => state.write_u8(6),
            Self::LoadingFailed {
                cause,
                return_to_sp,
            } => {
                state.write_u8(if *return_to_sp { 8 } else { 7 });
                cause.as_ptr().hash(state);
            }
            Self::Other(s) => {
                state.write_u8(9);
                s.hash(state);
            }
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, SubStates)]
#[source(ClientState = ClientState::LoadingFailed { .. })]
pub struct LoadingFailed;

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

pub fn track_state_changes(
    state: Res<State<ClientState>>,
    mut last_state: ResMut<LastState>,
    mut last: Local<ClientState>,
) {
    last_state.0 = std::mem::replace(&mut *last, state.clone());
}

pub fn render_main_menu(
    mut contexts: EguiContexts,
    mut next_state: ResMut<NextState<ClientState>>,
    config: Res<crate::ClientPlugin>,
    mut exit: EventWriter<AppExit>,
) {
    egui::Area::new(egui::Id::new("Main Menu"))
        .anchor(egui::Align2::CENTER_CENTER, egui::Vec2::ZERO)
        .show(contexts.ctx_mut(), |ui| {
            ui.set_style(config.egui_style.clone());
            egui::Frame::window(ui.style()).show(ui, |ui| {
                ui.with_layout(egui::Layout::top_down(egui::Align::Center), |ui| {
                    ui.set_width(200.0);
                    ui.heading("Factor");
                    if ui
                    .add_enabled(
                        config.can_use_singleplayer,
                        egui::Button::new(egui::RichText::new("Singleplayer").text_style(egui::TextStyle::Button)).min_size(egui::vec2(ui.max_rect().width(), 0.0)),
                    )
                    .on_disabled_hover_text(
                        "Singleplayer worlds are not supported here! Try using a different build.",
                    )
                    .clicked()
                {
                    next_state.set(ClientState::SPSelect);
                }
                    if ui
                        .add(
                            egui::Button::new(egui::RichText::new("Multiplayer").text_style(egui::TextStyle::Button)).min_size(egui::vec2(ui.max_rect().width(), 0.0))
                        )
                        .clicked()
                    {
                        next_state.set(ClientState::MPSelect);
                    }
                    if ui.add(
                        egui::Button::new(egui::RichText::new("Settings").text_style(egui::TextStyle::Button)).min_size(egui::vec2(ui.max_rect().width(), 0.0)),
                    ).clicked() {
                        next_state.set(ClientState::Settings);
                    }
                    if ui.add(
                        egui::Button::new(egui::RichText::new("Quit").text_style(egui::TextStyle::Button)).min_size(egui::vec2(ui.max_rect().width(), 0.0))
                    ).clicked() {
                        exit.send_default();
                    }
                });
            });
        });
}

pub fn render_mp_select(
    mut contexts: EguiContexts,
    mut next_state: ResMut<NextState<ClientState>>,
    config: Res<crate::ClientPlugin>,
    mut server: Local<String>,
) {
    egui::Area::new(egui::Id::new("Main Menu"))
        .anchor(egui::Align2::CENTER_CENTER, egui::Vec2::ZERO)
        .show(contexts.ctx_mut(), |ui| {
            ui.set_style(config.egui_style.clone());
            egui::Frame::window(ui.style()).show(ui, |ui| {
                ui.set_width(500.0);
                if ui.input(|input| input.key_pressed(egui::Key::Escape)) {
                    next_state.set(ClientState::MainMenu);
                }
                ui.with_layout(egui::Layout::top_down(egui::Align::Center), |ui| {
                    ui.heading("Multiplayer");
                });
                ui.with_layout(egui::Layout::right_to_left(egui::Align::Min), |ui| {
                    if ui.button("Back").clicked() {
                        next_state.set(ClientState::MainMenu);
                    }
                    if ui.button("Connect").clicked() {
                        let _uri = std::mem::take(&mut *server);
                        next_state.set(ClientState::WorldLoading);
                    }
                    ui.add(egui::TextEdit::singleline(&mut *server).desired_width(f32::INFINITY));
                });
            });
        });
}

pub fn render_settings(
    mut contexts: EguiContexts,
    config: Res<ClientPlugin>,
    last_state: Res<LastState>,
    mut next_state: ResMut<NextState<ClientState>>,
) {
    egui::Area::new(egui::Id::new("Settings"))
        .anchor(egui::Align2::CENTER_CENTER, egui::Vec2::ZERO)
        .show(contexts.ctx_mut(), |ui| {
            ui.set_style(config.egui_style.clone());
            egui::Frame::window(ui.style()).show(ui, |ui| {
                ui.with_layout(egui::Layout::top_down(egui::Align::Center), |ui| {
                    ui.set_width(500.0);
                    ui.label(egui::RichText::new("Settings").heading().size(40.0));

                    ui.label("No settings here yet, check back later!");

                    if ui.button("Back").clicked() {
                        next_state.set(last_state.clone());
                    }
                });
            });
        });
}

pub fn render_loading_failed(
    mut contexts: EguiContexts,
    config: Res<ClientPlugin>,
    state: Res<State<ClientState>>,
    mut next_state: ResMut<NextState<ClientState>>,
) {
    let ClientState::LoadingFailed {
        cause,
        return_to_sp,
    } = &**state
    else {
        unreachable!()
    };
    egui::Area::new(egui::Id::new("Loading Failed"))
        .anchor(egui::Align2::CENTER_CENTER, egui::Vec2::ZERO)
        .show(contexts.ctx_mut(), |ui| {
            ui.set_style(config.egui_style.clone());
            egui::Frame::window(ui.style()).show(ui, |ui| {
                ui.with_layout(egui::Layout::top_down(egui::Align::Center), |ui| {
                    ui.set_width(500.0);
                    ui.label(egui::RichText::new("Oh no!").heading().size(40.0));
                    ui.label("Failed to load the world!");
                    ui.label(
                        egui::RichText::new(cause.to_string())
                            .color(ui.style().visuals.error_fg_color)
                            .background_color(ui.style().visuals.extreme_bg_color),
                    );
                    if ui.button("Back").clicked() {
                        next_state.set(if *return_to_sp {
                            ClientState::SPSelect
                        } else {
                            ClientState::MPSelect
                        });
                    }
                });
            });
        });
}

pub fn render_loading(
    mut contexts: EguiContexts,
    config: Res<ClientPlugin>,
    time: Res<Time>,
    mut dots: Local<u32>,
    mut timer: Local<Option<Timer>>,
) {
    let ticks = timer
        .get_or_insert_with(|| Timer::new(Duration::from_secs(1), TimerMode::Repeating))
        .tick(time.elapsed())
        .times_finished_this_tick();
    *dots += ticks;
    *dots %= 4;
    egui::Area::new(egui::Id::new("Loading Failed"))
        .anchor(egui::Align2::CENTER_CENTER, egui::Vec2::ZERO)
        .show(contexts.ctx_mut(), |ui| {
            ui.set_style(config.egui_style.clone());
            let mut msg = "Loading".to_string();
            for _ in 0..*dots {
                msg.push('.');
            }
            ui.label(msg);
        });
}
