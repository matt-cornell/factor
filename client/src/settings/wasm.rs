use bevy::log::warn;

impl super::ClientSettings {
    pub fn load() -> Self {
        warn!("Saving/loading settings in WASM is a no-op for now!");
        Self::default()
    }
    pub fn save(&self) {
        warn!("Saving settings in WASM is a no-op for now!");
    }
}
