#![feature(trait_upcasting)]

use bevy::prelude::*;
use bevy_egui::{egui, EguiPlugin};
use core_ui::*;
use std::sync::{Arc, OnceLock};

pub mod core_ui;
pub mod net;

#[derive(Debug, Clone, Resource)]
pub struct ClientPlugin {
    pub can_use_singleplayer: bool,
    pub egui_style: Arc<egui::Style>,
}
impl Plugin for ClientPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(EguiPlugin)
            .init_state::<ClientState>()
            .insert_resource(LastState(ClientState::MainMenu))
            .insert_resource(self.clone())
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

static DEFAULT_STYLE: OnceLock<Arc<egui::Style>> = OnceLock::new();

pub fn default_style() -> Arc<egui::Style> {
    use egui::{
        FontFamily::{Monospace, Proportional},
        FontId, TextStyle,
    };
    DEFAULT_STYLE
        .get_or_init(|| {
            Arc::new(egui::Style {
                text_styles: [
                    (TextStyle::Small, FontId::new(15.0, Proportional)),
                    (TextStyle::Body, FontId::new(25.0, Proportional)),
                    (TextStyle::Button, FontId::new(25.0, Proportional)),
                    (TextStyle::Heading, FontId::new(40.0, Proportional)),
                    (TextStyle::Monospace, FontId::new(25.0, Monospace)),
                ]
                .into(),
                ..default()
            })
        })
        .clone()
}
