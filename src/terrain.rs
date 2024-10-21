use bevy::math::Vec2;
use rand::distributions::{Standard, Uniform};
use rand::prelude::*;
use std::f32::consts::*;

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
        TectonicPlate {
            center_lat: rng.gen_range(-1.0f32..1.0).asin(),
            center_long: rng.gen_range(-PI..=PI),
            height: rng.gen_range(-0.1..=0.1),
            dx: 0.0,
            dy: 0.0,
            scratch: 0.0,
            count: 0,
        }
    }
}

pub fn init_terrain<R: Rng + ?Sized>(
    depth: u8,
    rng: &mut R,
) -> (Box<[TectonicCell]>, Box<[TectonicPlate]>) {
    let noise = rand_distr::Normal::new(0.0, 0.15).unwrap();
    let layer = cdshealpix::nested::get(depth);
    let len = layer.n_hash() as _;
    let nplates = rng.gen_range(5..=8)
        * if depth == 0 {
            0
        } else {
            depth.ilog2() as usize
        };
    let mut changes = rng
        .sample_iter(noise)
        .array_chunks()
        .map(|[a, b]| Vec2::new(a, b))
        .take(len)
        .collect::<Box<[_]>>();
    let mut scratch = vec![Vec2::ZERO; len].into_boxed_slice();
    let mut neighbors = Vec::new();
    for _ in 0..5 {
        std::mem::swap(&mut scratch, &mut changes);
        for (n, v) in changes.iter_mut().enumerate() {
            neighbors.clear();
            layer.append_bulk_neighbours(n as _, &mut neighbors);
            *v *= 0.1;
            *v += neighbors.iter().map(|n| scratch[*n as usize]).sum::<Vec2>()
                / neighbors.len() as f32
                * 0.9;
        }
    }
    let mut plates: Box<[TectonicPlate]> = rng.sample_iter(Standard).take(nplates).collect();
    // let mut updated = true;
    // while updated {
    //     updated = false;
    //     for i in 0..nplates {
    //         let p = plates[i];
    //         let latc = p.scratch;
    //         let lats = (1.0 - latc.powi(2)).sqrt();
    //         let long = p.center_long;
    //         let closest = plates
    //             .iter()
    //             .enumerate()
    //             .map(|(i, p)| {
    //                 (
    //                     i as u8,
    //                     (lats * (1.0 - p.scratch.powi(2)).sqrt()
    //                         + latc * p.scratch * (p.center_long - long).cos())
    //                     .acos(),
    //                 )
    //             })
    //             .filter(|n| n.1 > 0.0)
    //             .min_by(|a, b| a.1.total_cmp(&b.1))
    //             .unwrap();
    //         if closest.1 > FRAC_PI_3 * 2.0 {
    //             plates[closest.0 as usize] = rng.gen();
    //             updated = true;
    //         }
    //     }
    // }
    let mut cells: Box<[TectonicCell]> = changes
        .iter()
        .enumerate()
        .map(|(n, delta)| {
            let (long, lat) = layer.center(n as _);
            let (lat, long) = (lat as f32, long as f32 + delta.x);
            let lat = (lat + PI) % TAU - PI + delta.y;
            let (plate, _) = plates
                .iter()
                .enumerate()
                .map(|(i, p)| {
                    (
                        i as u8,
                        (lat - p.center_lat).powi(2)
                            + (long - p.center_long)
                                .min(TAU - long + p.center_long)
                                .powi(2),
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
        layer.append_bulk_neighbours(i as _, &mut neighs);
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
