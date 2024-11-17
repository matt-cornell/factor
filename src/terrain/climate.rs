use crate::healpix;
use bevy::math::*;
use rand::prelude::*;
use rand_distr::Normal;

const BOLTZMANN_CONSTANT: f32 = 5.6703744;

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

pub fn setup_climate<F: FnMut(u64) -> f32, R: Rng + ?Sized>(
    depth: u8,
    mut heights: F,
    ocean_coverage: f32,
    rng: &mut R,
) -> Box<[ClimateCell]> {
    let len = healpix::n_hash(depth);
    let temp_sample: Normal<f32> = Normal::new(25.0, 0.5).unwrap();
    let heat_sample: Normal<f32> = Normal::new(0.15, 0.03).unwrap();
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
        cell.heat_capacity = 4.2;
        cell.albedo = 0.4;
        cell.ocean = true;
    }
    cells
}

pub fn step_climate<F: FnMut(f32, f32) -> f32, R: Rng + ?Sized>(
    cells: &mut [ClimateCell],
    mut solar_intensity: F,
    _rng: &mut R,
) {
    let depth = healpix::depth(cells.len() as u32 / 12);
    let layer = healpix::nested::get(depth);

    // radiation
    for (idx, cell) in cells.iter_mut().enumerate() {
        let (lon, lat) = layer.center(idx as _);
        let sun = solar_intensity(lon as _, lat as _);
        let energy = sun * cell.albedo - 0.9 * (cell.temp + 273.0).powi(4) * BOLTZMANN_CONSTANT;
        cell.temp += energy / cell.heat_capacity;
    }

    // conduction
    let scale = 0.8f32.powi(1 << depth.saturating_sub(2));
    for i in 0..cells.len() {
        let neighbors = healpix::neighbors(depth, i as _);
        let mut cum_temp = 0.0;
        let mut cum_humid = 0.0;
        let mut cum_wind = Vec2::ZERO;
        let (clon, clat) = layer.center(i as _);
        let (clon, clat) = (clon as f32, clat as f32);
        let (osin, ocos) = clon.sin_cos();
        let (asin, acos) = clat.sin_cos();
        let axis_x = Vec3::new(-osin, ocos, 0.0);
        let axis_y = Vec3::new(-asin * ocos, -asin * osin, acos);
        let center_3d = Vec3::new(ocos * acos, osin * asin, asin);
        let base_pressure = cells[i].temp.recip();
        for &n in neighbors {
            let cell = &cells[n as usize];
            cum_temp += cell.temp;
            cum_humid += cell.humidity;
            let pressure_diff = base_pressure - cell.temp.recip();
            let (clon, clat) = layer.center(n);
            let (clon, clat) = (clon as f32, clat as f32);
            let (osin, ocos) = clon.sin_cos();
            let (asin, acos) = clat.sin_cos();
            let center2 = Vec3::new(ocos * acos, osin * asin, asin);
            let diff = center2 - center_3d;
            let base = Vec2::new(diff.dot(axis_x), diff.dot(axis_y));
            cum_wind += base.normalize_or_zero() * pressure_diff;
        }
        cum_temp /= neighbors.len() as f32;
        cum_humid /= neighbors.len() as f32;
        let cell = &mut cells[i];
        cell.temp *= 1.0 - scale;
        cell.temp += cum_temp * scale;
        cell.humidity *= 1.0 - scale;
        cell.humidity += cum_humid * scale;
    }
}
