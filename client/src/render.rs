use std::f32::consts::FRAC_PI_2;

use crate::action::Action;
use crate::player::AttemptedMotion;
use crate::settings::ClientSettings;
use crate::ClientState;
use bevy::prelude::*;
use factor_common::data::{DefaultPlayer, Position};
use leafwing_input_manager::prelude::*;

pub fn setup_world_render(mut commands: Commands, pos: Single<&Position, DefaultPlayer>) {
    commands.spawn((Camera3d::default(), pos.get_transform()));
    commands.init_resource::<AttemptedMotion>();
}
pub fn pause_world_render(mut camera: Single<&mut Camera, With<Camera3d>>) {
    camera.is_active = false;
}
pub fn resume_world_render(mut camera: Single<&mut Camera, With<Camera3d>>) {
    camera.is_active = true;
}
pub fn cleanup_world_render(mut commands: Commands, camera: Single<Entity, With<Camera3d>>) {
    commands.entity(*camera).despawn();
}

pub fn handle_keypresses(
    state: Res<ActionState<Action>>,
    input: Res<ButtonInput<KeyCode>>,
    settings: Res<ClientSettings>,
    pos: Single<&Position, DefaultPlayer>,
    mut next_state: ResMut<NextState<ClientState>>,
    mut attempted: ResMut<AttemptedMotion>,
) {
    if input.just_pressed(KeyCode::Escape) {
        info!("Pausing");
        next_state.set(ClientState::Paused);
    }
    let mut walk = state.clamped_axis_pair(&Action::Move);
    let len2 = walk.length_squared();
    if len2 > 1.0 {
        walk /= len2.sqrt();
    }
    let (y, _x, _) = pos.rot.to_euler(EulerRot::YXZ);
    let mut look = state.clamped_axis_pair(&Action::Move);
    look *= settings.mouse_sensitivity;
    look.y = (look.y + y).clamp(-FRAC_PI_2, FRAC_PI_2) - y;
    let jump = state.just_pressed(&Action::Jump);
    *attempted = AttemptedMotion { walk, look, jump };
}

pub fn local_reflect_attempts(
    attempt: Res<AttemptedMotion>,
    mut pos: Single<&mut Position, DefaultPlayer>,
) {
    let Vec2 { x, y } = attempt.look;
    pos.rot = Quat::from_rotation_x(y) * Quat::from_rotation_y(x) * pos.rot;
    let mut forward = (pos.rot * -Vec3::Z).xz();
    if forward.length_squared() < 0.01 {
        forward = (pos.rot * Vec3::Y).xz();
    }
    forward = forward.normalize();
    pos.pos += attempt.walk.rotate(forward).extend(0.0).xzy();
    // TODO: jump/gravity
}
