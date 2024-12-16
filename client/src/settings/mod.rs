use crate::action::Action;
use bevy::prelude::*;
use leafwing_input_manager::prelude::*;
use serde::{Deserialize, Serialize};
use std::time::Duration;

#[cfg(target_family = "wasm")]
mod wasm;
#[cfg(target_family = "wasm")]
pub use wasm::*;

#[cfg(not(target_family = "wasm"))]
mod native;
#[cfg(not(target_family = "wasm"))]
pub use native::*;

/// Target framerate as a resource.
#[derive(Debug, Default, Clone, Copy, PartialEq, Resource)]
pub enum TargetFps {
    #[default]
    Unlimited,
    Limit(f32),
}
impl From<Option<f32>> for TargetFps {
    fn from(value: Option<f32>) -> Self {
        value.map_or(Self::Unlimited, Self::Limit)
    }
}

/// Run condition that limits a system to run on the given framerate.
pub fn with_fps(fps: Option<Res<TargetFps>>, time: Res<Time>, mut last: Local<Duration>) -> bool {
    let t = time.elapsed();
    let run = if let Some(&TargetFps::Limit(lim)) = fps.as_deref() {
        (t - *last).as_secs_f32().recip() < lim
    } else {
        true
    };
    if run {
        *last = t;
    }
    run
}

#[derive(Default, Deserialize)]
struct PartialClientSettings {
    render_distance: Option<f64>,
    target_fps: Option<Option<f32>>,
    mouse_sensitivity: Option<f32>,
}
impl PartialClientSettings {
    pub fn is_incomplete(&self) -> bool {
        self.render_distance.is_none()
            || self.target_fps.is_none()
            || self.mouse_sensitivity.is_none()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Resource)]
#[serde(from = "PartialClientSettings")]
pub struct ClientSettings {
    pub render_distance: f64,
    pub target_fps: Option<f32>,
    pub mouse_sensitivity: f32,
}
impl Default for ClientSettings {
    fn default() -> Self {
        Self {
            render_distance: 1000.0,
            target_fps: None,
            mouse_sensitivity: 0.01,
        }
    }
}
impl From<PartialClientSettings> for ClientSettings {
    fn from(value: PartialClientSettings) -> Self {
        Self {
            render_distance: value.render_distance.unwrap_or(1000.0),
            target_fps: value.target_fps.flatten(),
            mouse_sensitivity: value.mouse_sensitivity.unwrap_or(0.01),
        }
    }
}

#[derive(Serialize)]
struct SerializeShim<'a> {
    settings: &'a ClientSettings,
    input: &'a InputMap<Action>,
}

#[derive(Deserialize)]
struct DeserializeShim {
    settings: Option<PartialClientSettings>,
    input: Option<InputMap<Action>>,
}

pub fn fill_keybinds(map: &mut InputMap<Action>) -> bool {
    if map.get_dual_axislike(&Action::Move).is_none() {
        map.insert_dual_axis(Action::Move, VirtualDPad::wasd());
    }
    if map.get_dual_axislike(&Action::Look).is_none() {
        map.insert_dual_axis(Action::Look, MouseMove::default());
    }
    if map.get_buttonlike(&Action::Jump).is_none() {
        map.insert(Action::Jump, KeyCode::Space);
    }
    false
}
