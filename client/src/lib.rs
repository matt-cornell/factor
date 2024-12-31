#![feature(trait_upcasting, try_blocks)]

use bevy::prelude::*;
use bevy_egui::{egui, EguiPlugin};
use core_ui::*;
use std::sync::{Arc, OnceLock};

pub mod action;
pub mod chunks;
pub mod core_ui;
pub mod net;
pub mod player;
pub mod render;
pub mod settings;
pub mod utils;

#[derive(Debug, Clone, Resource)]
pub struct ClientPlugin {
    pub can_use_singleplayer: bool,
    pub egui_style: Arc<egui::Style>,
}
impl Plugin for ClientPlugin {
    fn build(&self, app: &mut App) {
        fn setup_target_fps(
            settings: Res<settings::ClientSettings>,
            mut fps: ResMut<settings::TargetFps>,
        ) {
            *fps = settings.target_fps;
        }
        fn limit_target_fps(mut fps: ResMut<settings::TargetFps>) {
            *fps = settings::TargetFps::Limit(10.0);
        }
        fn clear_motion(mut attempt: ResMut<player::AttemptedMotion>) {
            *attempt = default();
        }
        app.add_plugins(EguiPlugin)
            .add_plugins(leafwing_input_manager::plugin::InputManagerPlugin::<
                action::Action,
            >::default())
            .init_state::<ClientState>()
            .init_state::<WorldLoaded>()
            .add_sub_state::<LoadingFailed>()
            .add_sub_state::<RenderGame>()
            .insert_resource(LastState(ClientState::MainMenu))
            .insert_resource(self.clone())
            .add_systems(PreStartup, settings::load_config)
            .add_systems(
                Update,
                (
                    track_state_changes.run_if(state_changed::<ClientState>),
                    render_main_menu.run_if(in_state(ClientState::MainMenu)),
                    render_mp_select.run_if(in_state(ClientState::MPSelect)),
                    render_settings.run_if(in_state(ClientState::Settings)),
                    render_loading_failed.run_if(in_state(LoadingFailed)),
                    render_paused
                        .after(bevy_egui::EguiSet::InitContexts)
                        .run_if(in_state(ClientState::Paused)),
                    (
                        render::handle_keypresses,
                        render::local_reflect_attempts,
                        render::autopause,
                    )
                        .chain()
                        .run_if(in_state(ClientState::Running)),
                    (chunks::update_interest, render::link_camera)
                        .before(render_paused)
                        .run_if(in_state(RenderGame).and(settings::with_fps)),
                ),
            )
            .add_systems(OnEnter(WorldLoaded(true)), render::setup_world_render)
            .add_systems(OnExit(WorldLoaded(true)), render::cleanup_world_render)
            .add_systems(
                OnEnter(ClientState::Running),
                (
                    setup_target_fps,
                    render::resume_world_render,
                    render::grab_mouse,
                ),
            )
            .add_systems(
                OnExit(ClientState::Running),
                (
                    limit_target_fps,
                    clear_motion,
                    render::pause_world_render,
                    render::release_mouse,
                )
                    .before(render::cleanup_world_render),
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
