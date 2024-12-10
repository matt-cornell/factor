use bevy::prelude::Resource;
use serde::{Deserialize, Serialize};

#[cfg(target_family = "wasm")]
mod wasm;

#[cfg(not(target_family = "wasm"))]
mod native;

#[derive(Deserialize)]
struct PartialClientSettings {
    render_distance: Option<f64>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Resource)]
#[serde(from = "PartialClientSettings")]
pub struct ClientSettings {
    pub render_distance: f64,
}
impl Default for ClientSettings {
    fn default() -> Self {
        Self {
            render_distance: 1000.0,
        }
    }
}
impl From<PartialClientSettings> for ClientSettings {
    fn from(value: PartialClientSettings) -> Self {
        Self {
            render_distance: value.render_distance.unwrap_or(1000.0),
        }
    }
}
