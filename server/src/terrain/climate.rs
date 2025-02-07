use bevy::math::*;
use bytemuck::*;
use factor_common::healpix;
use rand::prelude::*;
use rand_distr::Normal;
use std::fmt::{self, Debug, Formatter};

const STEFAN_BOLTZMANN_CONSTANT: f32 = 5.6703744e-8;
const WATER_HEAT_CAPACITY: f32 = 630.0;
const ROCK_HEAT_CAPACITY: f32 = 22.5;
const ROCK_HEAT_CAPACITY_DEV: f32 = 4.5;
const WATER_GAS_CONSTANT: f32 = 461.5; // J/(kg K)
const WATER_CRIT_PRESSURE: f32 = 22064000.0; // Pa
const WATER_CRIT_TEMPERATURE: f32 = 647.096; // K

pub const START_TEMPERATURE: f32 = 20.0;

fn saturation_pressure(temp: f32) -> f32 {
    let t = temp.clamp(0.0, WATER_CRIT_TEMPERATURE - 1.0);
    // https://www.calctool.org/atmospheric-thermodynamics/absolute-humidity
    #[allow(clippy::excessive_precision)]
    const A: [f32; 6] = [
        -7.85951783,
        1.84408259,
        -11.7866497,
        22.6807411,
        -15.9618719,
        1.80122502,
    ];
    let tau = 1.0 - t / WATER_CRIT_TEMPERATURE;
    let sum = A[0] * tau
        + A[1] * tau.powf(1.5)
        + A[2] * tau.powi(3)
        + A[3] * tau.powf(3.5)
        + A[4] * tau.powi(4)
        + A[5] * tau.powf(7.5);
    WATER_CRIT_PRESSURE * (WATER_CRIT_TEMPERATURE / t * sum).exp()
}

macro_rules! gen_as_str {
    ($this:expr, $($name:ident)*) => {
        match $this {
            $(Self::$name => Ok(stringify!($name)),)*
            Self(id) => Err(id)
        }
    };
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash, Pod, Zeroable)]
#[repr(transparent)]
pub struct Biome(pub u32);
impl Biome {
    pub const PLACEHOLDER: Self = Self(0x0000);
    pub const PLAINS: Self = Self(0x0001);

    pub const WARM_SHALLOW0: Self = Self(0x1000);
    pub const WARM_SHALLOW1: Self = Self(0x1001);
    pub const WARM_SHALLOW2: Self = Self(0x1002);
    pub const WARM_SHALLOW3: Self = Self(0x1003);
    pub const WARM_DEEP_OCEAN: Self = Self(0x1004);

    pub const COLD_SHALLOW0: Self = Self(0x1005);
    pub const COLD_SHALLOW1: Self = Self(0x1006);
    pub const COLD_SHALLOW2: Self = Self(0x1007);
    pub const COLD_SHALLOW3: Self = Self(0x1008);
    pub const COLD_DEEP_OCEAN: Self = Self(0x1009);

    pub const ICE_SHALLOW0: Self = Self(0x100a);
    pub const ICE_SHALLOW1: Self = Self(0x100b);
    pub const ICE_SHALLOW2: Self = Self(0x100c);
    pub const ICE_SHALLOW3: Self = Self(0x100d);
    pub const ICE_DEEP_OCEAN: Self = Self(0x100e);

    pub const OCEANS: &[Self] = &[
        Self::WARM_DEEP_OCEAN,
        Self::WARM_SHALLOW0,
        Self::WARM_SHALLOW1,
        Self::WARM_SHALLOW2,
        Self::WARM_SHALLOW3,
        Self::COLD_DEEP_OCEAN,
        Self::COLD_SHALLOW0,
        Self::COLD_SHALLOW1,
        Self::COLD_SHALLOW2,
        Self::COLD_SHALLOW3,
        Self::ICE_DEEP_OCEAN,
        Self::ICE_SHALLOW0,
        Self::ICE_SHALLOW1,
        Self::ICE_SHALLOW2,
        Self::ICE_SHALLOW3,
    ];

    pub const fn as_str(&self) -> Result<&'static str, u32> {
        gen_as_str!(*self,
            PLACEHOLDER PLAINS
            WARM_DEEP_OCEAN WARM_SHALLOW0 WARM_SHALLOW1 WARM_SHALLOW2 WARM_SHALLOW3
            COLD_DEEP_OCEAN COLD_SHALLOW0 COLD_SHALLOW1 COLD_SHALLOW2 COLD_SHALLOW3
            ICE_DEEP_OCEAN ICE_SHALLOW0 ICE_SHALLOW1 ICE_SHALLOW2 ICE_SHALLOW3
        )
    }
    pub fn is_ocean(&self) -> bool {
        Self::OCEANS.contains(self)
    }
    pub fn with_new_climate(self, temp: f32, _humid: f32) -> Self {
        match self {
            Self::WARM_DEEP_OCEAN | Self::COLD_DEEP_OCEAN | Self::ICE_DEEP_OCEAN => match temp {
                ..=0.0 => Self::ICE_DEEP_OCEAN,
                0.0..=15.0 => Self::COLD_DEEP_OCEAN,
                15.0.. => Self::WARM_DEEP_OCEAN,
                _ => unreachable!(),
            },
            Self::WARM_SHALLOW3 | Self::COLD_SHALLOW3 | Self::ICE_SHALLOW3 => match temp {
                ..=0.0 => Self::ICE_SHALLOW3,
                0.0..=16.0 => Self::COLD_SHALLOW3,
                16.0.. => Self::WARM_SHALLOW3,
                _ => unreachable!(),
            },
            Self::WARM_SHALLOW2 | Self::COLD_SHALLOW2 | Self::ICE_SHALLOW2 => match temp {
                ..=0.0 => Self::ICE_SHALLOW2,
                0.0..=17.0 => Self::COLD_SHALLOW2,
                17.0.. => Self::WARM_SHALLOW2,
                _ => unreachable!(),
            },
            Self::WARM_SHALLOW1 | Self::COLD_SHALLOW1 | Self::ICE_SHALLOW1 => match temp {
                ..=0.0 => Self::ICE_SHALLOW1,
                0.0..=18.0 => Self::COLD_SHALLOW1,
                18.0.. => Self::WARM_SHALLOW1,
                _ => unreachable!(),
            },
            Self::WARM_SHALLOW0 | Self::COLD_SHALLOW0 | Self::ICE_SHALLOW0 => match temp {
                ..=0.0 => Self::ICE_SHALLOW0,
                0.0..=18.0 => Self::COLD_SHALLOW0,
                18.0.. => Self::WARM_SHALLOW0,
                _ => unreachable!(),
            },
            _ => self,
        }
    }
    pub fn update_new_climate(&mut self, temp: f32, humid: f32) {
        *self = self.with_new_climate(temp, humid);
    }
}
impl Debug for Biome {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.as_str() {
            Ok(s) => f.write_str(s),
            Err(id) => write!(f, "Biome({id})"),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, Pod, Zeroable)]
#[repr(C)]
pub struct ClimateCell {
    pub height: f32,
    pub temp: f32,
    pub humidity: f32,
    pub heat_capacity: f32,
    pub albedo: f32,
    pub rainfall: f32,
    pub wind: Vec2,
    pub biome: Biome,
}

impl redb::Value for ClimateCell {
    type SelfType<'a> = Self;
    type AsBytes<'a> = [u8; size_of::<ClimateCell>()];
    fn as_bytes<'a, 'b: 'a>(value: &'a Self::SelfType<'b>) -> Self::AsBytes<'a>
    where
        Self: 'a,
        Self: 'b,
    {
        let mut out = [0u8; 9 * 4];
        #[cfg(target_endian = "little")]
        out.copy_from_slice(bytes_of(value));
        #[cfg(target_endian = "big")]
        {
            out[0..4].copy_from_slice(&value.height.to_le_bytes());
            out[4..8].copy_from_slice(&value.temp.to_le_bytes());
            out[8..12].copy_from_slice(&value.humidity.to_le_bytes());
            out[12..16].copy_from_slice(&value.heat_capacity.to_le_bytes());
            out[16..20].copy_from_slice(&value.albedo.to_le_bytes());
            out[20..24].copy_from_slice(&value.rainfall.to_le_bytes());
            out[24..28].copy_from_slice(&value.wind.x.to_le_bytes());
            out[28..32].copy_from_slice(&value.wind.y.to_le_bytes());
            out[32..36].copy_from_slice(&value.biome.0 .0.to_le_bytes());
        }
        out
    }
    fn from_bytes<'a>(data: &'a [u8]) -> Self::SelfType<'a>
    where
        Self: 'a,
    {
        let mut this = Self::zeroed();
        #[cfg(target_endian = "little")]
        bytes_of_mut(&mut this).copy_from_slice(&data[..size_of::<ClimateCell>()]);
        #[cfg(target_endian = "big")]
        {
            for (&bytes, field) in data.array_chunks().zip([
                &mut this.height,
                &mut this.temp,
                &mut this.humidity,
                &mut this.heat_capacity,
                &mut this.albedo,
                &mut this.rainfall,
                &mut this.wind.x,
                &mut this.wind.y,
            ]) {
                *field = f32::from_le_bytes(bytes);
            }
            this.biome.0 .0 = u32::from_le_bytes(data[32..36].try_into().unwrap());
        }
        this
    }
    fn fixed_width() -> Option<usize> {
        Some(36)
    }
    fn type_name() -> redb::TypeName {
        redb::TypeName::new("factor::ClimateCell")
    }
}

pub fn init_climate<F: FnMut(u64) -> f32, R: Rng + ?Sized>(
    depth: u8,
    mut heights: F,
    ocean_coverage: f32,
    rng: &mut R,
) -> Box<[ClimateCell]> {
    let len = healpix::n_hash(depth);
    let temp_sample: Normal<f32> = Normal::new(START_TEMPERATURE, 0.5).unwrap();
    let heat_sample: Normal<f32> = Normal::new(ROCK_HEAT_CAPACITY, ROCK_HEAT_CAPACITY_DEV).unwrap();
    let albedo_sample: Normal<f32> = Normal::new(0.2, 0.05).unwrap();
    let wind_sample: Normal<f32> = Normal::new(0.0, 0.01).unwrap();
    let mut cells = (0..len)
        .map(|n| ClimateCell {
            height: heights(n),
            temp: rng.sample(temp_sample),
            humidity: 0.0,
            heat_capacity: rng.sample(heat_sample).abs(),
            albedo: rng.sample(albedo_sample).abs(),
            wind: Vec2::new(rng.sample(wind_sample), rng.sample(wind_sample)),
            rainfall: 0.0,
            biome: Biome::PLACEHOLDER,
        })
        .collect::<Box<_>>();
    let mut sorted = (0..(len as usize)).collect::<Box<_>>();
    sorted.sort_unstable_by(|&a, &b| cells[a].height.total_cmp(&cells[b].height));
    let num_ocean = (len as f32 * ocean_coverage) as usize;
    let ocean_indices = &sorted[..num_ocean];
    for &idx in ocean_indices {
        let cell = &mut cells[idx];
        cell.humidity = 0.8 * 13.8; // 80% RH
        cell.heat_capacity = WATER_HEAT_CAPACITY;
        cell.albedo = 0.4;
        cell.biome = Biome::WARM_DEEP_OCEAN;
    }
    for i in 0..=3 {
        let prev = if i == 0 {
            Biome::PLACEHOLDER
        } else {
            Biome(Biome::WARM_SHALLOW0.0 + i - 1)
        };
        let new = Biome(Biome::WARM_SHALLOW0.0 + i);
        for &idx in ocean_indices {
            if cells[idx].biome == Biome::WARM_DEEP_OCEAN
                && healpix::Layer::new(depth)
                    .neighbors_slice(idx as _, true)
                    .iter()
                    .any(|i| cells[*i as usize].biome == prev)
            {
                cells[idx].biome = new;
            }
        }
    }
    cells
}

pub fn step_climate<F: FnMut(f32, f32) -> f32, R: Rng + ?Sized>(
    cells: &mut [ClimateCell],
    mut solar_intensity: F,
    time_scale: f32,
    rng: &mut R,
) {
    let temp_sample = Normal::new(0.0f32, 2.0).unwrap();
    let humid_sample = Normal::new(0.0f32, 0.1).unwrap();
    let depth = {
        let per_square = cells.len() as u32 / 12;
        debug_assert_eq!(per_square.count_ones(), 1);
        per_square.trailing_zeros() as u8 / 2
    };
    let layer = healpix::nested::get(depth);
    let mut old = cells.to_vec().into_boxed_slice();

    // radiation
    let mut avg_rainfall = 0.0;
    for (idx, cell) in cells.iter_mut().enumerate() {
        let (lon, lat) = layer.center(idx as _);
        let sun = solar_intensity(lon as _, lat as _);
        let energy =
            sun * cell.albedo - 0.9 * (cell.temp + 273.15).powi(4) * STEFAN_BOLTZMANN_CONSTANT;
        cell.temp += (energy / cell.heat_capacity * time_scale).max(-200.0);
        avg_rainfall += cell.rainfall;
        cell.rainfall *= 0.2; // clean this up too
    }
    avg_rainfall /= cells.len() as f32;

    let scale = 1.0 - (1.0 - 0.995f32.powi(1 << depth.saturating_sub(2))) * time_scale;

    // #[cfg(debug_assertions)]
    if let Some(n) = cells.iter().position(|c| !c.humidity.is_finite()) {
        panic!("NaN humidity in cell {n}:\n{:#?}", cells[n]);
    }

    // convection?
    let num_iters = 1 << depth.saturating_sub(3);
    {
        let scale = scale / num_iters as f32 * 0.6;
        for _ in 0..num_iters {
            for (i, old) in old.iter().enumerate() {
                let neighbors = healpix::neighbors(depth, i as _, true);
                let (clon, clat) = layer.center(i as _);
                let (clon, clat) = (clon as f32, clat as f32);
                let (asin1, acos1) = clat.sin_cos();
                for &n in &neighbors {
                    let cell = &mut cells[n as usize];
                    let (lon, lat) = layer.center(n);
                    let (lon, lat) = (lon as f32, lat as f32);
                    let (asin2, acos2) = lat.sin_cos();
                    let (osin, ocos) = (clon - lon).sin_cos();
                    let base = Vec2::new(osin * acos2, acos1 * asin2 - asin1 * acos2 * ocos);
                    let dot = base.normalize_or_zero().dot(old.wind);
                    cell.wind += old.wind.normalize() * dot.abs() * 0.1;
                    if dot > 0.1 {
                        let k = (dot.sqrt() * 0.01).clamp(0.0, 1.0);
                        cell.temp *= 1.0 - scale * k;
                        cell.temp += old.temp * scale * k;
                        cell.humidity *= 1.0 - scale * k.min(old.humidity);
                        cell.humidity += old.humidity * scale * k.min(old.humidity);

                        let temp = cell.temp.clamp(-100.0, 100.0);
                        let humid = cell.humidity.clamp(0.0, 200.0);
                        let cell = &mut cells[i];
                        cell.temp *= 1.0 - scale * 0.5 * k;
                        cell.temp += temp * scale * 0.5 * k;
                        cell.temp += rng.sample(temp_sample).max(-cell.temp);
                        cell.humidity *= 1.0 - scale * k;
                        cell.humidity += humid * scale * k;
                        cell.humidity *= rng.sample(humid_sample).max(-cell.humidity).exp();
                        cell.biome.with_new_climate(cell.temp, cell.humidity);
                    }
                }
            }
            old.copy_from_slice(cells);
        }
    }

    // conduction
    for _ in 0..(num_iters * 3 / 2) {
        for (i, cell) in cells.iter_mut().enumerate() {
            let neighbors = healpix::neighbors(depth, i as _, true);
            let mut cum_temp = 0.0;
            let mut cum_humid = 0.0;
            let mut cum_rain = 0.0;
            let mut cum_wind = Vec2::ZERO;
            let (clon, clat) = layer.center(i as _);
            let (clon, clat) = (clon as f32, clat as f32);
            let (asin1, acos1) = clat.sin_cos();
            let base_pressure = cell.temp;
            for &n in &neighbors {
                let cell = &old[n as usize];
                cum_temp += cell.temp;
                cum_humid += cell.humidity;
                cum_rain += cell.rainfall;
                let pressure_diff = base_pressure - cell.temp;
                let (lon, lat) = layer.center(n);
                let (lon, lat) = (lon as f32, lat as f32);
                let (asin2, acos2) = lat.sin_cos();
                let (osin, ocos) = (clon - lon).sin_cos();
                let base = Vec2::new(osin * acos2, acos1 * asin2 - asin1 * acos2 * ocos);
                cum_wind += base.normalize_or_zero() * pressure_diff * scale * scale;
            }
            cum_temp /= neighbors.len() as f32;
            cum_humid /= neighbors.len() as f32;
            cum_rain /= neighbors.len() as f32;
            assert_ne!(neighbors.len(), 0);
            cell.temp *= 1.0 - scale * 0.5;
            cell.temp += cum_temp * scale * 0.5;
            cell.humidity *= 1.0 - scale * 0.1;
            cell.humidity += cum_humid * scale * 0.1;
            cell.wind *= 1.0 - scale;
            cell.wind += cum_wind * scale;
            cell.rainfall *= 1.0 - scale * 0.5;
            cell.rainfall += cum_rain * scale * 0.5;
        }
        old.copy_from_slice(cells);
    }

    for cell in &mut *cells {
        let max_humidity = saturation_pressure(cell.temp + 273.15)
            / (WATER_GAS_CONSTANT * (cell.temp + 273.15))
            * 100.0;
        if cell.biome.is_ocean() && cell.humidity < max_humidity {
            cell.humidity += (max_humidity - cell.humidity) * 0.05;
        }
        let humid_diff = (cell.humidity - max_humidity).max(0.0).powi(2);
        cell.humidity -= humid_diff * 0.1;
        cell.rainfall *= 0.09;
        cell.rainfall += humid_diff * 1.0;
        cell.rainfall += cell.humidity
            * 1.5
            * (rng.gen::<f32>().powi(2) + (cell.humidity * 0.1).clamp(0.0, 1.0));
    }

    // rain clustering
    for _ in 0..(num_iters / 2) {
        for (i, cell) in cells.iter_mut().enumerate() {
            let neighbors = healpix::neighbors(depth, i as _, true);
            let mut cum_humid = 0.0;
            let mut cum_rain = 0.0;
            for &n in &neighbors {
                let cell = &old[n as usize];
                cum_humid += cell.humidity;
                cum_rain += cell.rainfall;
            }
            cum_humid /= neighbors.len() as f32;
            cum_rain /= neighbors.len() as f32;
            cell.humidity = cell.humidity.clamp(0.0, 100.0);
            cell.humidity *= 1.0 - scale * 0.1;
            cell.humidity += cum_humid * scale * 0.1;
            cell.rainfall *= 1.0 - scale * 0.5;
            cell.rainfall += cum_rain * scale * 0.5;
        }
        old.copy_from_slice(cells);
    }
    let mut total_rain = 0.0;
    let mut div = 0.0;
    for (i, cell) in cells.iter_mut().enumerate() {
        let prob = cell.rainfall as f64 * 0.05
            + healpix::Layer::new(depth)
                .neighbors_slice(i as _, true)
                .into_iter()
                .map(|n| old[n as usize].rainfall as f64 * 0.1)
                .sum::<f64>();
        if rng.gen_bool(prob.clamp(0.1, 0.85)) {
            cell.rainfall =
                1.1f32.powf(cell.rainfall).min(20.0) / avg_rainfall.clamp(0.2, 1.5) * 50.0;
            total_rain += cell.rainfall;
            div += 1.0;
        } else {
            cell.rainfall = 0.0;
            div += 5.0;
        }
    }
    for _ in 0..(num_iters * 2) {
        for (i, cell) in cells.iter_mut().enumerate() {
            let neighbors = healpix::neighbors(depth, i as _, true);
            let mut cum_humid = 0.0;
            let mut cum_rain = 0.0;
            for &n in &neighbors {
                let cell = &old[n as usize];
                cum_humid += cell.humidity;
                cum_rain += cell.rainfall;
            }
            cum_humid /= neighbors.len() as f32;
            cum_rain /= neighbors.len() as f32;
            cell.humidity = cell.humidity.clamp(0.0, 100.0);
            cell.humidity *= 1.0 - scale * 0.1;
            cell.humidity += cum_humid * scale * 0.1;
            cell.rainfall *= 1.0 - scale * 0.5;
            cell.rainfall += cum_rain * scale * 0.5;
        }
        old.copy_from_slice(cells);
    }

    let avg_rain = total_rain / div;
    for cell in cells.iter_mut() {
        cell.rainfall = (cell.rainfall - avg_rain).max(0.0);
    }
}
