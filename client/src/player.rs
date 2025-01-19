use bevy::prelude::*;

#[derive(Debug, Default, Clone, Copy, PartialEq, Resource)]
pub struct AttemptedMotion {
    pub walk: Vec2,
    pub jump: bool,
    pub crouch: bool,
    pub look: Vec2,
}
