use bevy::asset::*;
use bevy::prelude::*;
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// A single layer of value or Perlin noise
#[derive(Debug, Clone, Copy, Reflect, Serialize, Deserialize)]
pub struct NoiseLayer {
    /// HEALPix depth to sample at. Values above 7 make it freeze!
    pub depth: u8,
    /// Scale to multiply the noise result by.
    pub scale: f32,
    /// Longitude shift to apply before we sample the noise.
    pub shift: f32,
    /// Should we use gradient noise? If set to false, value noise is used instead.
    pub gradient: bool,
}

/// Parameters for the noise used to texture the surface.
#[derive(Debug, Clone, Reflect, Serialize, Deserialize)]
pub struct NoiseConfig {
    /// Global noise layers- these should be low-octave noises for the whole planet.
    pub global: Vec<NoiseLayer>,
    /// Local noise layers- these are for texturing local terrain, and can have a higher octave.
    pub local: Vec<NoiseLayer>,
}

/// Parameters for the tectonic simulation.
#[derive(Debug, Clone, Copy, Reflect, Serialize, Deserialize)]
pub struct TectonicConfig {
    /// HEALPix depth to use for simulation. 5 works well here.
    pub depth: u8,
    /// Number of steps to use for the simulation. Generally between 50-200 works well.
    pub steps: u16,
}

/// Parameters for the planet's orbit.
#[derive(Debug, Clone, Copy, Reflect, Serialize, Deserialize)]
pub struct OrbitConfig {
    /// Length of a day in seconds.
    pub day_length: f32,
    /// Length of a year in seconds.
    pub year_length: f32,
    /// Orbital obliquity in radians.
    pub obliquity: f32,
}

/// Configuration that's used to create the world.
#[derive(Debug, Clone, Asset, Reflect, Serialize, Deserialize)]
pub struct WorldConfig {
    /// Random seed to use for terrain generation.
    #[serde(with = "crate::utils::option_bytes")]
    pub seed: Option<[u8; 32]>,
    /// See [`OrbitConifg`].
    pub orbit: OrbitConfig,
    /// See [`TectonicConfig`].
    pub tectonics: TectonicConfig,
    /// See [`NoiseConfig`].
    pub noise: NoiseConfig,
}

#[derive(Debug, Clone, Copy)]
pub struct WorldConfigLoader;
impl AssetLoader for WorldConfigLoader {
    type Asset = WorldConfig;
    type Error = WorldLoadError;
    type Settings = ();

    fn extensions(&self) -> &[&str] {
        &["toml"]
    }
    fn load<'a>(
        &'a self,
        reader: &'a mut io::Reader,
        _settings: &'a Self::Settings,
        _load_context: &'a mut LoadContext,
    ) -> impl bevy::utils::ConditionalSendFuture<Output = Result<Self::Asset, Self::Error>> {
        async move {
            let mut buf = Vec::new();
            reader.read_to_end(&mut buf).await?;
            let data = String::from_utf8(buf)?;
            let config = toml::from_str(&data)?;
            Ok(config)
        }
    }
}

/// An error that occurred when loading a world.
#[derive(Debug, Error)]
pub enum WorldLoadError {
    /// An IO error occurred.
    #[error(transparent)]
    Io(#[from] std::io::Error),
    /// The file we loaded wasn't UTF-8.
    #[error(transparent)]
    Utf8(#[from] std::string::FromUtf8Error),
    /// Invalid TOML, or missing fields.
    #[error(transparent)]
    Toml(#[from] toml::de::Error),
}
