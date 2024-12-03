use super::climate::*;
use super::noise::*;
use super::tectonic::*;
use crate::config::*;
use crate::orbit::*;
use crate::utils::database::*;
use bevy::prelude::*;
use factor_common::healpix;
use rand::prelude::*;
use rand_xoshiro::Xoshiro256PlusPlus as RandSource;

const TERRAIN_TABLE: TableDefinition<u64, ClimateCell> = TableDefinition::new("terrain");

#[derive(Debug, Clone)]
enum ValueOrGradient {
    Value(ValueCellNoise<Box<[f32]>, Identity>),
    Gradient(GradientCellNoise<Box<[Vec2]>, Identity>),
}
impl NoiseSource for ValueOrGradient {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        match self {
            Self::Value(v) => v.get_height(lon, lat),
            Self::Gradient(g) => g.get_height(lon, lat),
        }
    }
}

#[derive(Debug, Clone, Resource)]
pub struct ClimateData {
    pub cells: Box<[ClimateCell]>,
    pub depth: u8,
    pub min_temp: f32,
    pub max_temp: f32,
    pub med_temp: f32,
    pub avg_temp: f32,
    pub max_wind: f32,
    pub avg_wind: f32,
    pub max_rain: f32,
    pub avg_rain: f32,
}

/// Set up the terrain according to the parameters.
pub fn setup_terrain(
    mut commands: Commands,
    mut config: ResMut<WorldConfig>,
    mut center: Query<&mut Transform, With<PlanetCenter>>,
    mut planet: Query<
        (&mut Transform, &mut PlanetSurface, &GlobalTransform),
        Without<PlanetCenter>,
    >,
    database: Option<Res<Database>>,
) {
    let seed = *config.seed.get_or_insert_with(|| thread_rng().gen());
    let config = Res::from(config);
    let mut rng = RandSource::from_seed(seed);
    let mut tect_state = init_terrain(config.tectonics.depth, &mut rng);
    for _ in 0..config.tectonics.steps {
        step_terrain(&mut tect_state, &mut rng);
    }
    let noise = config
        .noise
        .global
        .iter()
        .map(|layer| {
            let base = if layer.gradient {
                ValueOrGradient::Gradient(GradientCellNoise {
                    depth: layer.depth,
                    hasher: thread_rng()
                        .sample_iter(rand_distr::UnitCircle)
                        .map(|[x, y]| Vec2::new(x, y))
                        .take(healpix::n_hash(layer.depth) as _)
                        .collect::<Box<[Vec2]>>(),
                    scale: Identity,
                })
            } else {
                ValueOrGradient::Value(ValueCellNoise {
                    depth: layer.depth,
                    hasher: thread_rng()
                        .sample_iter(rand_distr::Standard)
                        .take(healpix::n_hash(layer.depth) as _)
                        .collect::<Box<[f32]>>(),
                    scale: Identity,
                })
            };
            (Shifted::new(base, layer.shift), layer.scale)
        })
        .collect::<Vec<_>>();
    let climate_depth = config.climate.depth;
    let mut climate = init_climate(
        climate_depth,
        |cell| {
            let (lon, lat) = healpix::nested::center(climate_depth, cell);
            let tect_cell = healpix::nested::hash(config.tectonics.depth, lon, lat);
            tect_state.cells()[tect_cell as usize].height * config.tectonics.scale
                + noise.get_height(lon as _, lat as _)
        },
        0.7,
        &mut rng,
    );
    let dt = 2.0 * config.orbit.year_length / config.climate.init_steps as f32;
    for _ in 0..config.climate.init_steps {
        update_planet_transforms(
            In(dt),
            center.transmute_lens_filtered().query(),
            planet.transmute_lens_filtered().query(),
            Res::clone(&config),
        );
        let planet_transform = planet.single().2;
        step_climate(
            &mut climate,
            |x, y| {
                let (xsin, xcos) = x.sin_cos();
                let (ysin, ycos) = y.sin_cos();
                let point = Vec3::new(xcos * ycos, xsin * ycos, ysin);
                let (_, rot, trans) = planet_transform.to_scale_rotation_translation();
                config.climate.intensity
                    * rot.mul_vec3(point).dot(-trans.normalize_or_zero()).max(0.0)
            },
            config.climate.time_step * dt,
            &mut rng,
        );
    }
    let cells = climate.clone();
    let mut buffer = climate;
    buffer.sort_by(|a, b| a.temp.total_cmp(&b.temp));
    let l = buffer.len();
    let min_temp = buffer[0].temp;
    let max_temp = buffer[l - 1].temp;
    let med_temp = buffer[l / 2].temp;
    let [max_rain, max_wind, tot_temp, tot_rain, tot_wind] = buffer.iter().fold(
        [0.0f32; 5],
        |[max_rain, max_wind, tot_temp, tot_rain, tot_wind], cell| {
            let wind = cell.wind.length();
            [
                max_rain.max(cell.rainfall),
                max_wind.max(wind),
                tot_temp + cell.temp,
                tot_rain + cell.rainfall,
                tot_wind + wind,
            ]
        },
    );
    let avg_temp = tot_temp / l as f32;
    let avg_rain = tot_rain / l as f32;
    let avg_wind = tot_wind / l as f32;
    if let Some(db) = database {
        match db.begin_write() {
            Ok(txn) => {
                let erred = match txn.open_table(TERRAIN_TABLE) {
                    Ok(mut table) => 'save: {
                        for (n, cell) in cells.iter().enumerate() {
                            if let Err(err) = table.insert(n as u64, cell) {
                                error!(%err, hash = n, "Error saving cell to table");
                                break 'save true;
                            }
                        }
                        false
                    }
                    Err(err) => {
                        error!(%err, "Error opening terrain table");
                        true
                    }
                };
                #[allow(clippy::collapsible_else_if)]
                if erred {
                    if let Err(err) = txn.abort() {
                        error!(%err, "Subsequent error on rollback");
                    }
                } else {
                    if let Err(err) = txn.commit() {
                        error!(%err, "Error committing save");
                    }
                }
            }
            Err(err) => {
                error!(%err, "Error starting transaction");
            }
        }
    }
    commands.insert_resource(ClimateData {
        depth: climate_depth,
        cells,
        min_temp,
        max_temp,
        med_temp,
        avg_temp,
        max_wind,
        avg_wind,
        max_rain,
        avg_rain,
    });
}

pub fn update_climate(
    database: Option<Res<Database>>,
    mut climate: ResMut<ClimateData>,
    config: Res<WorldConfig>,
    planet: Query<&GlobalTransform, With<PlanetSurface>>,
    time_step: Time<Virtual>,
) {
    let planet_transform = planet.single();
    step_climate(
        &mut climate.cells,
        |x, y| {
            let (xsin, xcos) = x.sin_cos();
            let (ysin, ycos) = y.sin_cos();
            let point = Vec3::new(xcos * ycos, xsin * ycos, ysin);
            let (_, rot, trans) = planet_transform.to_scale_rotation_translation();
            config.climate.intensity * rot.mul_vec3(point).dot(-trans.normalize_or_zero()).max(0.0)
        },
        config.climate.time_step * time_step.delta_seconds(),
        &mut thread_rng(),
    );
    let mut buffer = climate.cells.clone();
    buffer.sort_by(|a, b| a.temp.total_cmp(&b.temp));
    let l = buffer.len();
    climate.min_temp = buffer[0].temp;
    climate.max_temp = buffer[l - 1].temp;
    climate.med_temp = buffer[l / 2].temp;
    let [max_rain, max_wind, tot_temp, tot_rain, tot_wind] = buffer.iter().fold(
        [0.0f32; 5],
        |[max_rain, max_wind, tot_temp, tot_rain, tot_wind], cell| {
            let wind = cell.wind.length();
            [
                max_rain.max(cell.rainfall),
                max_wind.max(wind),
                tot_temp + cell.temp,
                tot_rain + cell.rainfall,
                tot_wind + wind,
            ]
        },
    );
    climate.avg_temp = tot_temp / l as f32;
    climate.avg_rain = tot_rain / l as f32;
    climate.avg_wind = tot_wind / l as f32;
    climate.max_rain = max_rain;
    climate.max_wind = max_wind;
    if let Some(db) = database {
        match db.begin_write() {
            Ok(txn) => {
                let erred = match txn.open_table(TERRAIN_TABLE) {
                    Ok(mut table) => 'save: {
                        for (n, cell) in climate.cells.iter().enumerate() {
                            if let Err(err) = table.insert(n as u64, cell) {
                                error!(%err, hash = n, "Error saving cell to table");
                                break 'save true;
                            }
                        }
                        false
                    }
                    Err(err) => {
                        error!(%err, "Error opening terrain table");
                        true
                    }
                };
                #[allow(clippy::collapsible_else_if)]
                if erred {
                    if let Err(err) = txn.abort() {
                        error!(%err, "Subsequent error on rollback");
                    }
                } else {
                    if let Err(err) = txn.commit() {
                        error!(%err, "Error committing save");
                    }
                }
            }
            Err(err) => {
                error!(%err, "Error starting transaction");
            }
        }
    }
}
