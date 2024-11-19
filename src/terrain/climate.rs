use crate::healpix;
use bevy::math::*;
use rand::prelude::*;
use rand_distr::Normal;

const STEFAN_BOLTZMANN_CONSTANT: f32 = 5.6703744e-8;
const WATER_HEAT_CAPACITY: f32 = 630.0;
const ROCK_HEAT_CAPACITY: f32 = 22.5;
const ROCK_HEAT_CAPACITY_DEV: f32 = 4.5;

pub const START_TEMPERATURE: f32 = 20.0;

#[derive(Debug, Clone, Copy)]
pub struct ClimateCell {
    pub height: f32,
    pub temp: f32,
    pub humidity: f32,
    pub ocean: bool,
    pub heat_capacity: f32,
    pub albedo: f32,
    pub wind: Vec2,
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
            ocean: false,
            heat_capacity: rng.sample(heat_sample).abs(),
            albedo: rng.sample(albedo_sample).abs(),
            wind: Vec2::new(rng.sample(wind_sample), rng.sample(wind_sample)),
        })
        .collect::<Box<_>>();
    let mut sorted = (0..(len as usize)).collect::<Box<_>>();
    sorted.sort_unstable_by(|&a, &b| cells[a].height.total_cmp(&cells[b].height));
    let num_ocean = (len as f32 * ocean_coverage) as usize;
    for &idx in &sorted[..num_ocean] {
        let cell = &mut cells[idx];
        cell.humidity = 0.8;
        cell.heat_capacity = WATER_HEAT_CAPACITY;
        cell.albedo = 0.4;
        cell.ocean = true;
    }
    cells
}

pub fn step_climate<F: FnMut(f32, f32) -> f32, R: Rng + ?Sized>(
    cells: &mut [ClimateCell],
    mut solar_intensity: F,
    time_scale: f32,
    _rng: &mut R,
) -> Box<[ClimateCell]> {
    let depth = {
        let per_square = cells.len() as u32 / 12;
        debug_assert_eq!(per_square.count_ones(), 1);
        per_square.trailing_zeros() as u8 / 2
    };
    let layer = healpix::nested::get(depth);
    let old = cells.to_vec();

    // radiation
    for (idx, cell) in cells.iter_mut().enumerate() {
        let (lon, lat) = layer.center(idx as _);
        let sun = solar_intensity(lon as _, lat as _);
        let energy =
            sun * cell.albedo - 0.9 * (cell.temp + 273.15).powi(4) * STEFAN_BOLTZMANN_CONSTANT;
        cell.temp += (energy / cell.heat_capacity * time_scale).max(-200.0);
    }

    // conduction
    let scale = 1.0 - (1.0 - 0.995f32.powi(1 << depth.saturating_sub(2))) * time_scale;
    for (i, cell) in cells.iter_mut().enumerate() {
        let neighbors = healpix::neighbors(depth, i as _);
        let mut cum_temp = 0.0;
        let mut cum_humid = 0.0;
        let mut cum_wind = Vec2::ZERO;
        let (clon, clat) = layer.center(i as _);
        let (clon, clat) = (clon as f32, clat as f32);
        let (asin1, acos1) = clat.sin_cos();
        let base_pressure = cell.temp.recip();
        for &n in neighbors {
            let cell = &old[n as usize];
            cum_temp += cell.temp;
            cum_humid += cell.humidity;
            let pressure_diff = base_pressure - cell.temp.recip();
            let (lon, lat) = layer.center(n);
            let (lon, lat) = (lon as f32, lat as f32);
            let (asin2, acos2) = lat.sin_cos();
            let (osin, ocos) = (clon - lon).sin_cos();
            let base = Vec2::new(osin * acos2, acos1 * asin2 - asin1 * acos2 * ocos);
            cum_wind += base.normalize_or_zero() * pressure_diff;
        }
        cum_temp /= neighbors.len() as f32;
        cum_humid /= neighbors.len() as f32;
        cell.temp *= 1.0 - scale;
        cell.temp += cum_temp * scale;
        cell.humidity *= 1.0 - scale;
        cell.humidity += cum_humid * scale;
    }

    old.into_boxed_slice()
}
