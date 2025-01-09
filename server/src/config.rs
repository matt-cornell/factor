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
    /// Amount to scale tectonic depth by.
    pub scale: f32,
}

/// Parameters for the climate simulation
#[derive(Debug, Clone, Copy, Reflect, Serialize, Deserialize)]
pub struct ClimateConfig {
    /// HEALPix depth for the climate
    pub depth: u8,
    /// Intensity of the sunlight- 6000 works well for this
    pub intensity: f32,
    /// Initial number of steps.
    #[serde(alias = "init-steps")]
    pub init_steps: u16,
    /// Time step to be used in the simulation.
    /// Relates orbital parameters to thermal ones.
    #[serde(rename = "time-step")]
    pub time_step: f32,
}

/// Parameters for the planet's orbit.
#[derive(Debug, Clone, Copy, Reflect, Serialize, Deserialize)]
pub struct OrbitConfig {
    /// Length of a day in seconds.
    #[serde(alias = "day-length")]
    pub day_length: f32,
    /// Length of a year in seconds.
    #[serde(alias = "year-length")]
    pub year_length: f32,
    /// Orbital obliquity in radians.
    #[serde(alias = "axial-tilt", alias = "axial_tilt", alias = "tilt")]
    pub obliquity: f32,
}

/// Configuration that's used to create the world.
#[derive(Debug, Clone, Asset, Resource, Reflect, Serialize, Deserialize)]
pub struct WorldConfig {
    /// Random seed to use for terrain generation.
    #[serde(default, with = "crate::utils::option_bytes")]
    pub seed: Option<[u8; 32]>,
    /// Depth-12 frame to spawn in, should most likely be left to `None` for a suitable one to be chosen during creation.
    pub spawn: Option<u64>,
    /// See [`OrbitConifg`].
    pub orbit: OrbitConfig,
    /// See [`ClimateConfig`].
    pub climate: ClimateConfig,
    /// See [`TectonicConfig`].
    pub tectonics: TectonicConfig,
    /// See [`NoiseConfig`].
    pub noise: NoiseConfig,
}

static DEFAULT_CONFIG_FILE: &str = include_str!("default-config.toml");

impl Default for WorldConfig {
    fn default() -> Self {
        toml::from_str(DEFAULT_CONFIG_FILE).unwrap()
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct WorldConfigLoader;
impl AssetLoader for WorldConfigLoader {
    type Asset = WorldConfig;
    type Error = WorldLoadError;
    type Settings = ();

    fn extensions(&self) -> &[&str] {
        &["toml"]
    }
    fn load(
        &self,
        reader: &mut dyn io::Reader,
        _settings: &Self::Settings,
        _load_context: &mut LoadContext,
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
