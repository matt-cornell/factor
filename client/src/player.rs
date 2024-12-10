use bevy::prelude::*;

#[derive(Debug, Clone, Copy, Resource)]
pub struct PlayerPos {
    pub chunk: u64,
    pub position: Vec3,
}
