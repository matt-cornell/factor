use bevy::prelude::*;

#[derive(Debug, Clone, Copy, Resource)]
pub struct PlayerPos {
    pub chunk: u64,
    pub position: Vec3,
    pub rotation: Quat,
}
impl PlayerPos {
    pub fn get_isometry(&self) -> Isometry3d {
        Isometry3d::new(self.position, self.rotation)
    }
    pub fn get_transform(&self) -> Transform {
        Transform {
            translation: self.position,
            rotation: self.rotation,
            scale: Vec3::splat(1.0),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Resource)]
pub struct AttemptedMotion {
    pub walk: Vec2,
    pub jump: bool,
    pub look: Vec2,
}
