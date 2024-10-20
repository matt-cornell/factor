use rand::distributions::{Standard, Uniform};
use rand::prelude::*;
use std::f32::consts::{FRAC_PI_3, PI, TAU};

/// A single cell for the plate tectonics
#[derive(Debug, Default, Clone, Copy)]
pub struct TectonicCell {
    pub plate: u8,
    pub height: f32,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct TectonicPlate {
    pub center_lat: f32,
    pub center_long: f32,
    pub height: f32,
    pub dx: f32,
    pub dy: f32,
    pub scratch: f32,
    pub count: u32,
}

impl Distribution<TectonicPlate> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> TectonicPlate {
        let lat = rng.gen_range(-1.0f32..1.0).asin();
        TectonicPlate {
            center_lat: lat,
            center_long: rng.gen_range(-PI..=PI),
            height: rng.gen_range(-0.1..=0.1),
            dx: 0.0,
            dy: 0.0,
            scratch: lat.cos(),
            count: 0,
        }
    }
}

pub fn init_terrain<R: Rng + ?Sized>(
    depth: u8,
    rng: &mut R,
) -> (Box<[TectonicCell]>, Box<[TectonicPlate]>) {
    let layer = cdshealpix::nested::get(depth);
    let len = layer.n_hash() as _;
    let nplates = rng.gen_range(5..=8) * depth.ilog2() as usize;
    let mut plates: Box<[TectonicPlate]> = rng.sample_iter(Standard).take(nplates).collect();
    let mut updated = true;
    while updated {
        updated = false;
        for i in 0..nplates {
            let p = plates[i];
            let latc = p.scratch;
            let lats = (1.0 - latc.powi(2)).sqrt();
            let long = p.center_long;
            let closest = plates
                .iter()
                .enumerate()
                .map(|(i, p)| {
                    (
                        i as u8,
                        (lats * (1.0 - p.scratch.powi(2)).sqrt()
                            + latc * p.scratch * (p.center_long - long).cos())
                        .acos(),
                    )
                })
                .min_by(|a, b| a.1.total_cmp(&b.1))
                .unwrap();
            if closest.1 > FRAC_PI_3 * 2.0 {
                plates[closest.0 as usize] = rng.gen();
                updated = true;
            }
        }
    }
    let mut cells: Box<[TectonicCell]> = (0..len)
        .map(|n| {
            let (lat, long) = layer.center(n);
            let (lat, long) = (lat as f32, long as f32);
            let (lats, latc) = lat.sin_cos();
            let (plate, _) = plates
                .iter()
                .enumerate()
                .map(|(i, p)| {
                    (
                        i as u8,
                        (lats * (1.0 - p.scratch.powi(2)).sqrt()
                            + latc * p.scratch * (p.center_long - long).cos())
                        .acos()
                            % TAU,
                    )
                })
                .min_by(|a, b| a.1.total_cmp(&b.1))
                .unwrap();
            let cell = TectonicCell {
                plate,
                height: rng.gen_range(-0.025..=0.025),
            };
            plates[plate as usize].count += 1;
            cell
        })
        .collect();
    let mut neighs = Vec::new();
    for i in 0..len {
        let plate = cells[i as usize].plate;
        neighs.clear();
        layer.append_bulk_neighbours(i, &mut neighs);
        if !neighs.iter().any(|n| cells[*n as usize].plate == plate) {
            let new = cells[*neighs.choose(rng).unwrap() as usize].plate;
            plates[plate as usize].count -= 1;
            plates[new as usize].count += 1;
            cells[i as usize].plate = new;
        }
    }
    for cell in &mut cells {
        cell.height += plates[cell.plate as usize].height;
    }
    (cells, plates)
}
pub fn step_terrain<R: Rng + ?Sized>(
    depth: u8,
    terrain: &mut [TectonicCell],
    plates: &mut [TectonicPlate],
    rng: &mut R,
) {
    let layer = cdshealpix::nested::get(depth);
    debug_assert_eq!(layer.n_hash(), terrain.len() as u64);
    let currents = Uniform::new(-0.05, 0.05);
}
