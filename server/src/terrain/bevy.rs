use super::climate::*;
use super::noise::*;
use super::tectonic::*;
use crate::config::*;
use crate::orbit::*;
use crate::tables::{DataEntry, MISC_DATA, TERRAIN};
use crate::utils::database::*;
use crate::utils::db_value::ErasedValue;
use bevy::prelude::*;
use bevy::tasks::AsyncComputeTaskPool;
use crossbeam_channel::Receiver;
use factor_common::healpix;
use factor_common::util::UpdateStates;
use rand::prelude::*;
use rand_xoshiro::Xoshiro256PlusPlus as RandSource;
use std::io;

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
    pub sorted: Box<[ClimateCell]>,
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
impl ClimateData {
    pub fn new(cells: Box<[ClimateCell]>) -> Self {
        let depth = {
            let per_square = cells.len() as u32 / 12;
            debug_assert_eq!(per_square.count_ones(), 1);
            per_square.trailing_zeros() as u8 / 2
        };
        let mut sorted = cells.clone();
        sorted.sort_unstable_by(|a, b| a.temp.total_cmp(&b.temp));
        let l = sorted.len();
        let min_temp = sorted[0].temp;
        let max_temp = sorted[l - 1].temp;
        let med_temp = sorted[l / 2].temp;
        let [max_rain, max_wind, tot_temp, tot_rain, tot_wind] = sorted.iter().fold(
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
        Self {
            depth,
            cells,
            sorted,
            min_temp,
            max_temp,
            med_temp,
            avg_temp,
            max_wind,
            avg_wind,
            max_rain,
            avg_rain,
        }
    }
    pub fn update(&mut self) {
        self.sorted.clone_from(&self.cells);
        self.sorted
            .sort_unstable_by(|a, b| a.temp.total_cmp(&b.temp));
        let l = self.sorted.len();
        self.min_temp = self.sorted[0].temp;
        self.max_temp = self.sorted[l - 1].temp;
        self.med_temp = self.sorted[l / 2].temp;
        let [max_rain, max_wind, tot_temp, tot_rain, tot_wind] = self.sorted.iter().fold(
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
        self.max_rain = max_rain;
        self.max_wind = max_wind;
        self.avg_temp = tot_temp / l as f32;
        self.avg_rain = tot_rain / l as f32;
        self.avg_wind = tot_wind / l as f32;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Resource)]
pub struct ClimateRunning(pub bool);

/// Where we are in the initial setup. This is `None` if we aren't working on it.
/// This is all managed by this module, and shouldn't be externally modified.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, States)]
pub enum ClimatePhase {
    #[default]
    None,
    NoiseSetup,
    TectSetup(u16),
    TectStep(u16),
    ClimateSetup,
    ClimateStep(u16),
    Finalize,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, SubStates)]
#[source(ClimatePhase = ClimatePhase::TectSetup(_))]
pub struct SetupTectonics;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, SubStates)]
#[source(ClimatePhase = ClimatePhase::TectStep(_))]
pub struct RunningTectonics;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, SubStates)]
#[source(ClimatePhase = ClimatePhase::ClimateStep(_))]
pub struct RunningClimate;

#[derive(Debug, Clone, Resource)]
pub(crate) struct ClimateInit {
    rng: RandSource,
}

#[derive(Debug, Resource)]
pub(crate) struct ClimateNoise(Vec<(Shifted<ValueOrGradient>, f32)>);

#[derive(Debug, Resource)]
pub(crate) struct TectonicData {
    state: TectonicState,
}

#[derive(Debug, Event)]
pub struct TerrainReady {
    pub res: Result<(), redb::Error>,
}

pub fn setup_terrain(
    mut commands: Commands,
    mut config: ResMut<WorldConfig>,
    #[cfg(debug_assertions)] state: Res<State<ClimatePhase>>,
    mut next_state: ResMut<NextState<ClimatePhase>>,
) {
    info!("Beginning terrain setup");
    #[cfg(debug_assertions)]
    assert_eq!(
        **state,
        ClimatePhase::None,
        "attempted to run multiple climate simulations at once!"
    );
    let seed = *config.seed.get_or_insert_with(|| thread_rng().gen());
    let rng = RandSource::from_seed(seed);
    next_state.set(ClimatePhase::NoiseSetup);
    commands.insert_resource(ClimateInit { rng });
}

pub(crate) fn setup_noise(
    mut commands: Commands,
    mut init: ResMut<ClimateInit>,
    config: Res<WorldConfig>,
    mut next_state: ResMut<NextState<ClimatePhase>>,
) {
    debug!("Setting up noise");
    let noise = config
        .noise
        .global
        .iter()
        .map(|layer| {
            let base = if layer.gradient {
                ValueOrGradient::Gradient(GradientCellNoise {
                    depth: layer.depth,
                    hasher: rand_distr::UnitCircle
                        .sample_iter(&mut init.rng)
                        .map(|[x, y]| Vec2::new(x, y))
                        .take(healpix::n_hash(layer.depth) as _)
                        .collect::<Box<[Vec2]>>(),
                    scale: Identity,
                })
            } else {
                ValueOrGradient::Value(ValueCellNoise {
                    depth: layer.depth,
                    hasher: rand_distr::Standard
                        .sample_iter(&mut init.rng)
                        .take(healpix::n_hash(layer.depth) as _)
                        .collect::<Box<[f32]>>(),
                    scale: Identity,
                })
            };
            (Shifted::new(base, layer.shift), layer.scale)
        })
        .collect::<Vec<_>>();
    commands.insert_resource(ClimateNoise(noise));
    next_state.set(ClimatePhase::TectSetup(0));
}

pub(crate) fn setup_tect(
    mut commands: Commands,
    mut init: ResMut<ClimateInit>,
    config: Res<WorldConfig>,
    state: Res<State<ClimatePhase>>,
    mut next_state: ResMut<NextState<ClimatePhase>>,
    mut channel: Local<Option<Receiver<Result<(TectonicState, ClimateInit), u16>>>>,
) {
    let ClimatePhase::TectSetup(mut step) = **state else {
        unreachable!()
    };
    if step == 0 && channel.is_none() {
        debug!("Beginning tectonics setup");
    }
    trace!(step, "Tectonics setup");
    if let Some(rx) = &mut *channel {
        match rx.try_iter().last() {
            Some(Ok((state, init_))) => {
                *init = init_;
                commands.insert_resource(TectonicData { state });
                next_state.set(ClimatePhase::TectStep(0));
            }
            Some(Err(step)) => next_state.set(ClimatePhase::TectSetup(step + 1)),
            None => {}
        }
    } else {
        let depth = config.tectonics.depth;
        let mut init = init.clone();
        let (tx, rx) = crossbeam_channel::bounded(16);
        *channel = Some(rx);
        AsyncComputeTaskPool::get()
            .spawn(async move {
                loop {
                    step += 1;
                    #[allow(clippy::collapsible_else_if)]
                    if let Some(state) = try_init_terrain(depth, &mut init.rng) {
                        if let Err(_err) = tx.send(Ok((state, init))) {
                            error!("Channel is closed!");
                        }
                        break;
                    } else {
                        if let Err(_err) = tx.send(Err(step)) {
                            error!("Channel is closed!");
                        }
                    }
                }
            })
            .detach();
    }
}

pub(crate) fn run_tect(
    mut tect: ResMut<TectonicData>,
    mut init: ResMut<ClimateInit>,
    config: Res<WorldConfig>,
    state: Res<State<ClimatePhase>>,
    mut next_state: ResMut<NextState<ClimatePhase>>,
) {
    let ClimatePhase::TectStep(step) = **state else {
        unreachable!()
    };
    if step == 0 {
        debug!("Beginning tectonics simulation");
    }
    trace!(step, "Tectonics step");
    if step >= config.tectonics.steps {
        next_state.set(ClimatePhase::ClimateSetup);
    } else {
        step_terrain(&mut tect.state, &mut init.rng);
        next_state.set(ClimatePhase::TectStep(step + 1));
    }
}

pub(crate) fn setup_climate(
    mut commands: Commands,
    mut init: ResMut<ClimateInit>,
    noise: Res<ClimateNoise>,
    tect: Res<TectonicData>,
    config: Res<WorldConfig>,
    mut next_state: ResMut<NextState<ClimatePhase>>,
) {
    debug!("Setting up climate");
    let climate_depth = config.climate.depth;
    let height = ValueCellNoise {
        depth: config.tectonics.depth,
        scale: Identity,
        hasher: |cell: usize| tect.state.cells()[cell].height,
    };
    let cells = init_climate(
        climate_depth,
        |cell| {
            let (lon, lat) = healpix::nested::center(climate_depth, cell);
            height.get_height(lon as _, lat as _) * config.tectonics.scale
                + noise.0.get_height(lon as _, lat as _)
        },
        0.7,
        &mut init.rng,
    );
    commands.remove_resource::<ClimateNoise>();
    commands.remove_resource::<TectonicData>();
    commands.insert_resource(ClimateData::new(cells));
    next_state.set(ClimatePhase::ClimateStep(0));
}

pub(crate) fn run_climate(
    config: Res<WorldConfig>,
    mut init: ResMut<ClimateInit>,
    mut climate: ResMut<ClimateData>,
    mut center: Query<&mut Transform, With<PlanetCenter>>,
    mut planet: Query<
        (&mut Transform, &mut PlanetSurface, &GlobalTransform),
        Without<PlanetCenter>,
    >,
    state: Res<State<ClimatePhase>>,
    mut next_state: ResMut<NextState<ClimatePhase>>,
    mut channel: Local<Option<Receiver<(ClimateData, ClimateInit)>>>,
) {
    let ClimatePhase::ClimateStep(step) = **state else {
        unreachable!()
    };
    if step == 0 && channel.is_none() {
        debug!("Beginning climate simulation");
    }
    if let Some(rx) = &mut *channel {
        if let Ok(data) = rx.try_recv() {
            *channel = None;
            *climate = data.0;
            *init = data.1;
            next_state.set(ClimatePhase::ClimateStep(step + 1));
        } else {
            return;
        }
    }
    trace!(step, "Climate step");
    if step >= config.climate.init_steps {
        next_state.set(ClimatePhase::Finalize);
    } else {
        let dt = 20.0 * config.orbit.day_length / config.climate.init_steps as f32;
        update_planet_transforms(
            In(dt),
            center.transmute_lens_filtered().query(),
            planet.transmute_lens_filtered().query(),
            Res::clone(&config),
        );
        let planet_transform = *planet.single().2;
        let mut climate = climate.clone();
        let time_scale = config.climate.time_step * dt;
        let intensity = config.climate.intensity;
        let mut init = init.clone();
        let (tx, rx) = crossbeam_channel::bounded(1);
        *channel = Some(rx);
        AsyncComputeTaskPool::get()
            .spawn(async move {
                step_climate(
                    &mut climate.cells,
                    |x, y| {
                        let (xsin, xcos) = x.sin_cos();
                        let (ysin, ycos) = y.sin_cos();
                        let point = Vec3::new(xcos * ycos, xsin * ycos, ysin);
                        let (_, rot, trans) = planet_transform.to_scale_rotation_translation();
                        intensity * rot.mul_vec3(point).dot(-trans.normalize_or_zero()).max(0.0)
                    },
                    time_scale,
                    &mut init.rng,
                );
                std::future::ready(()).await;
                climate.update();
                if let Err(_err) = tx.send((climate, init)) {
                    error!("Somehow this channel closed?");
                }
            })
            .detach();
    }
}

pub(crate) fn finalize(
    config: Res<WorldConfig>,
    climate: Res<ClimateData>,
    mut commands: Commands,
    database: Option<Res<Database>>,
    mut next_state: ResMut<NextState<ClimatePhase>>,
) {
    let res = if let Some(db) = database {
        debug!("Saving to database");
        db.begin_write()
            .inspect_err(|err| error!(%err, "Error starting transaction"))
            .map_err(redb::Error::from)
            .and_then(|txn| {
                let erred: Result<(), redb::Error> = try {
                    let mut table = txn
                        .open_table(TERRAIN)
                        .inspect_err(|err| error!(%err, "Error opening terrain table"))?;
                    for (n, cell) in climate.cells.iter().enumerate() {
                        table.insert(n as u64, cell).inspect_err(
                            |err| error!(%err, hash = n, "Error saving cell to table"),
                        )?;
                    }
                    let mut table = txn
                        .open_table(MISC_DATA)
                        .inspect_err(|err| error!(%err, "Error opening misc data table"))?;
                    table
                        .insert(DataEntry::WorldConfig, ErasedValue::new(&*config))
                        .inspect_err(|err| error!(%err, "Error saving world config"))?;
                };
                #[allow(clippy::collapsible_else_if)]
                if let Err(err) = erred {
                    if let Err(err) = txn.abort() {
                        error!(%err, "Subsequent error on rollback");
                        Err(err.into())
                    } else {
                        Err(err)
                    }
                } else {
                    if let Err(err) = txn.commit() {
                        error!(%err, "Error committing save");
                        Err(err.into())
                    } else {
                        Ok(())
                    }
                }
            })
    } else {
        Ok(())
    };
    commands.remove_resource::<ClimateInit>();
    commands.queue(UpdateStates);
    commands.trigger(TerrainReady { res });
    next_state.set(ClimatePhase::None);
}

pub fn load_terrain(mut commands: Commands, db: Res<Database>) {
    let res: Result<(), redb::Error> = try {
        let txn = db.begin_read()?;
        let config = txn.open_table(MISC_DATA)?;
        let config = config.get(DataEntry::WorldConfig)?.ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                "World config not present in database",
            )
        })?;
        commands.insert_resource(
            config
                .value()
                .get::<WorldConfig>()
                .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?,
        );
        let table = txn.open_table(TERRAIN)?;
        let len = table.len()?;
        let mut cells = Vec::with_capacity(len as _);
        for (n, res) in table.iter()?.enumerate() {
            let (k, v) = res.inspect_err(|err| error!(n, %err, "Error loading cell"))?;
            if k.value() != n as u64 {
                error!(cell = n, "Missing cell value");
                commands.trigger(TerrainReady {
                    res: Err(redb::Error::Io(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Missing cell value",
                    ))),
                });
                return;
            }
            cells.push(v.value());
        }
        let per_square = cells.len() as u32 / 12;
        if per_square.count_ones() != 1 {
            error!(len = cells.len(), "Invalid number of healpix cells");
            commands.trigger(TerrainReady {
                res: Err(redb::Error::Io(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Invalid number of healpix cells",
                ))),
            });
            return;
        }
        commands.insert_resource(ClimateData::new(cells.into_boxed_slice()));
    };
    if let Err(err) = &res {
        error!(%err, "Error loading table");
    }
    commands.trigger(TerrainReady { res });
}

pub fn update_climate(
    database: Option<Res<Database>>,
    mut climate: ResMut<ClimateData>,
    config: Res<WorldConfig>,
    planet: Query<&GlobalTransform, With<PlanetSurface>>,
    time_step: Res<Time<Virtual>>,
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
        config.climate.time_step * time_step.delta_secs(),
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
                let erred = match txn.open_table(TERRAIN) {
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
