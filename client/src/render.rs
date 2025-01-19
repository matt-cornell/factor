use crate::action::Action;
use crate::player::AttemptedMotion;
use crate::settings::{ClientSettings, DebugSettings};
use crate::ClientState;
use bevy::ecs::system::SystemChangeTick;
use bevy::pbr::wireframe::{Wireframe, WireframeConfig};
use bevy::prelude::*;
use bevy::window::{CursorGrabMode, PrimaryWindow};
use factor_common::data::{DefaultPlayer, Position};
use leafwing_input_manager::prelude::*;
use std::f32::consts::{FRAC_PI_2, PI, TAU};

pub fn setup_world_render(mut commands: Commands, pos: Single<&Position, DefaultPlayer>) {
    commands.spawn((
        PointLight {
            intensity: 4000.0,
            color: Color::WHITE,
            ..default()
        },
        Transform::from_xyz(0.0, 100.0, 0.0),
    ));
    let mut transform = pos.get_transform();
    transform.translation.y += 1.5;
    commands.spawn((Camera3d::default(), transform));
    commands.init_resource::<AttemptedMotion>();
}
pub fn pause_world_render(mut camera: Single<&mut Camera, With<Camera3d>>) {
    camera.is_active = false;
}
pub fn resume_world_render(mut camera: Single<&mut Camera, With<Camera3d>>) {
    camera.is_active = true;
}
pub fn cleanup_world_render(mut commands: Commands, camera: Single<Entity, With<Camera3d>>, wireframes: Option<ResMut<WireframeConfig>>) {
    commands.entity(*camera).despawn();
    if let Some(mut wireframes) = wireframes {
        wireframes.global = false;
    }
}

pub fn handle_keypresses(
    state: Res<ActionState<Action>>,
    input: Res<ButtonInput<KeyCode>>,
    settings: Res<ClientSettings>,
    mut next_state: ResMut<NextState<ClientState>>,
    mut attempted: ResMut<AttemptedMotion>,
    debug: Option<ResMut<DebugSettings>>,
    wireframes: Option<ResMut<WireframeConfig>>,
    window: Single<&Window, With<PrimaryWindow>>,
) {
    let scale = window.width().min(window.height());
    if input.just_pressed(KeyCode::Escape) {
        info!("Pausing");
        next_state.set(ClientState::Paused);
    }
    if let Some(mut debug) = debug {
        #[allow(clippy::collapsible_if)]
        if input.pressed(KeyCode::Backslash) {
            if input.just_pressed(KeyCode::KeyF) {
                if let Some(mut wireframes) = wireframes {
                    info!("Toggling wireframes");
                    debug.wireframes = !debug.wireframes;
                    // wireframes.global = debug.wireframes;
                } else {
                    warn!("Wireframes are disabled for this target");
                }
            }
        }
    }
    let mut walk = state.clamped_axis_pair(&Action::Move);
    let len2 = walk.length_squared();
    if len2 > 1.0 {
        walk /= len2.sqrt();
    }
    let mut look = -state.clamped_axis_pair(&Action::Look);
    look *= settings.mouse_sensitivity * scale;
    let jump = state.just_pressed(&Action::Jump);
    *attempted = AttemptedMotion { walk, look, jump };
}

pub fn local_reflect_attempts(
    attempt: Res<AttemptedMotion>,
    mut pos: Single<&mut Position, DefaultPlayer>,
) {
    let Vec2 { x, y } = attempt.look;
    let (y0, x0, z0) = pos.rot.to_euler(EulerRot::YXZ);
    let y0 = (y0 + PI) % TAU - PI;
    pos.rot = Quat::from_euler(
        EulerRot::YXZ,
        y0 + x,
        (x0 + y).clamp(-FRAC_PI_2, FRAC_PI_2),
        z0,
    );
}

pub fn link_camera(
    tick: SystemChangeTick,
    mut trans: Single<&mut Transform, With<Camera3d>>,
    pos: Single<Ref<Position>, DefaultPlayer>,
) {
    if pos
        .last_changed()
        .is_newer_than(trans.last_changed(), tick.this_run())
    {
        **trans = pos.get_transform();
        trans.translation.y += 1.5;
    }
}

pub fn autopause(
    window: Single<&Window>,
    state: Res<State<ClientState>>,
    mut next_state: ResMut<NextState<ClientState>>,
) {
    if !window.focused && **state == ClientState::Running {
        next_state.set(ClientState::Paused);
    }
}

pub fn grab_mouse(mut window: Single<&mut Window>) {
    window.cursor_options.grab_mode = CursorGrabMode::Confined;
    window.cursor_options.visible = false;
}

pub fn release_mouse(mut window: Single<&mut Window>) {
    window.cursor_options.grab_mode = CursorGrabMode::None;
    window.cursor_options.visible = true;
}
