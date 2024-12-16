use std::f32::consts::FRAC_PI_2;

use crate::action::Action;
use crate::player::{AttemptedMotion, PlayerPos};
use crate::settings::ClientSettings;
use crate::ClientState;
use bevy::prelude::*;
use leafwing_input_manager::prelude::*;

pub fn setup_world_render(mut commands: Commands, pos: Res<PlayerPos>) {
    commands.spawn((Camera3d::default(), pos.get_transform()));
    commands.init_resource::<AttemptedMotion>();
}

pub fn handle_keypresses(
    state: Res<ActionState<Action>>,
    input: Res<ButtonInput<KeyCode>>,
    settings: Res<ClientSettings>,
    pos: Res<PlayerPos>,
    mut next_state: ResMut<NextState<ClientState>>,
    mut attempted: ResMut<AttemptedMotion>,
) {
    if input.pressed(KeyCode::Escape) {
        info!("Pausing");
        next_state.set(ClientState::Paused);
    }
    let mut walk = state.clamped_axis_pair(&Action::Move);
    let len2 = walk.length_squared();
    if len2 > 1.0 {
        walk /= len2.sqrt();
    }
    let (y, _x, _) = pos.rotation.to_euler(EulerRot::YXZ);
    let mut look = state.clamped_axis_pair(&Action::Move);
    look *= settings.mouse_sensitivity;
    look.y = (look.y + y).clamp(-FRAC_PI_2, FRAC_PI_2) - y;
    let jump = state.just_pressed(&Action::Jump);
    *attempted = AttemptedMotion { walk, look, jump };
}

pub fn local_reflect_attempts(attempt: Res<AttemptedMotion>, mut pos: ResMut<PlayerPos>) {
    let Vec2 { x, y } = attempt.look;
    pos.rotation = Quat::from_rotation_x(y) * Quat::from_rotation_y(x) * pos.rotation;
    let mut forward = (pos.rotation * -Vec3::Z).xz();
    if forward.length_squared() < 0.01 {
        forward = (pos.rotation * Vec3::Y).xz();
    }
    forward = forward.normalize();
    pos.position += attempt.walk.rotate(forward).extend(0.0).xzy();
    // TODO: jump/gravity
}
