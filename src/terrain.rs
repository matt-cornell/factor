use bevy::math::Vec2;
use rand::distributions::Standard;
use rand::prelude::*;
use std::f32::consts::*;
use std::sync::OnceLock;
use tinyset::SetU64;

static NEIGHBORS: [OnceLock<Box<[u64]>>; 29] = [const { OnceLock::new() }; 29];
#[allow(clippy::type_complexity)]
static FAR_NEIGHBORS: [OnceLock<Box<[OnceLock<Box<[u64]>>]>>; 29] = [const { OnceLock::new() }; 29]; // TODO: precompute and save to a file
pub fn neighbors_list(depth: u8) -> &'static [u64] {
    NEIGHBORS[depth as usize].get_or_init(|| {
        let layer = cdshealpix::nested::get(depth);
        let len = layer.n_hash();
        let mut data = Vec::with_capacity(len as usize * 4);
        for i in 0..len {
            layer.append_bulk_neighbours(i, &mut data);
            let new_len = (i as usize + 1) * 4;
            debug_assert!(
                data.len() <= new_len,
                "more than four neighbors for cell {i}"
            );
            data.resize(new_len, u64::MAX);
        }
        data.into_boxed_slice()
    })
}
pub fn neighbors(depth: u8, hash: u64) -> &'static [u64] {
    let max_slice = &neighbors_list(depth)[(hash as usize * 4)..(hash as usize * 4 + 4)];
    max_slice
        .split_once(|x| *x == u64::MAX)
        .map_or(max_slice, |x| x.0)
}
pub fn far_neighbors(depth: u8, hash: u64) -> &'static [u64] {
    FAR_NEIGHBORS[depth as usize].get_or_init(|| {
        vec![OnceLock::new(); cdshealpix::n_hash(depth) as usize].into_boxed_slice()
    })[hash as usize]
        .get_or_init(|| {
            let mut set = Vec::new();
            let mut edge = vec![hash];
            for _ in 0..(1 << depth.saturating_sub(3)) {
                let end = set.len();
                set.append(&mut edge);
                for &i in &set[end..] {
                    edge.extend(
                        neighbors(depth, i)
                            .iter()
                            .copied()
                            .filter(|e| !set.contains(e)),
                    );
                }
            }
            set.sort_unstable();
            set.dedup();
            set.into_boxed_slice()
        })
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CellFeatures {
    #[default]
    None,
    Ridge,
    Subduction,
    Mountain,
}

/// A single cell for the plate tectonics
#[derive(Debug, Default, Clone, Copy)]
pub struct TectonicCell {
    pub plate: u8,
    pub height: f32,
    pub density: u32,
    pub feats: CellFeatures,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct TectonicPlate {
    pub center_lat: f32,
    pub center_long: f32,
    pub height: f32,
    pub density: u32,
    pub motion: Vec2,
    pub scratch: f32,
    pub count: u32,
}

impl Distribution<TectonicPlate> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> TectonicPlate {
        TectonicPlate {
            center_lat: rng.gen_range(-1.0f32..1.0).asin(),
            center_long: rng.gen_range(-PI..=PI),
            height: rng.gen_range(-0.1..=0.1),
            density: rng.gen_range(1000..=2000),
            motion: Vec2::ZERO,
            scratch: 0.0,
            count: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TerrainState {
    depth: u8,
    cells: Box<[TectonicCell]>,
    plates: Box<[TectonicPlate]>,
    boundaries: SetU64,
}
impl TerrainState {
    pub fn cells(&self) -> &[TectonicCell] {
        &self.cells
    }
    pub fn plates(&self) -> &[TectonicPlate] {
        &self.plates
    }
    pub fn boundaries(&self) -> &SetU64 {
        &self.boundaries
    }
}

pub fn init_terrain<R: Rng + ?Sized>(depth: u8, rng: &mut R) -> TerrainState {
    let noise = rand_distr::Normal::new(0.0, 0.5).unwrap();
    let layer = cdshealpix::nested::get(depth);
    let len = layer.n_hash() as _;
    let nplates = rng.gen_range(6..=9);
    let mut changes = rng
        .sample_iter(noise)
        .array_chunks()
        .map(|[a, b]| Vec2::new(a, b))
        .take(len)
        .collect::<Box<[_]>>();
    let mut scratch = vec![Vec2::ZERO; len].into_boxed_slice();
    for _ in 0..(6 + (1 << (depth - 2))) {
        std::mem::swap(&mut scratch, &mut changes);
        for (n, v) in changes.iter_mut().enumerate() {
            let neighs = neighbors(depth, n as _);
            *v *= 0.2;
            *v += neighs.iter().map(|n| scratch[*n as usize]).sum::<Vec2>() / neighs.len() as f32
                * 0.8;
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
                density: rng.gen_range(100..=1000),
                feats: CellFeatures::None,
            };
            plates[plate as usize].count += 1;
            cell
        })
        .collect();
    let mut boundaries = SetU64::new();
    for i in 0..len {
        let plate = cells[i].plate;
        let neighs = neighbors(depth, i as _);
        let mut seen_same = false;
        let mut seen = SetU64::new();
        for &n in neighs {
            let p = cells[n as usize].plate;
            if p == plate {
                seen_same = true;
                if seen.len() > 1 {
                    break;
                }
            } else {
                seen.insert(n);
                if seen.len() > 1 {
                    boundaries.insert(i as _);
                    if seen_same {
                        break;
                    }
                }
            }
        }
        if !seen_same {
            let new = cells[*neighs.choose(rng).unwrap() as usize].plate;
            plates[plate as usize].count -= 1;
            plates[new as usize].count += 1;
            cells[i].plate = new;
        }
    }
    for cell in &mut cells {
        let plate = &plates[cell.plate as usize];
        cell.height += plate.height;
        cell.density += plate.density;
    }
    TerrainState {
        depth,
        cells,
        plates,
        boundaries,
    }
}
pub fn step_terrain<R: Rng + ?Sized>(state: &mut TerrainState, rng: &mut R) {
    let layer = cdshealpix::nested::get(state.depth);
    debug_assert_eq!(layer.n_hash(), state.cells.len() as u64);
    for i in state.boundaries.iter() {
        let cell = state.cells[i as usize];
        let plate = state.plates[cell.plate as usize];
        let set = far_neighbors(state.depth, i);
        for (c2, p2) in set.iter().filter_map(|&n| {
            let plate = state.cells[n as usize].plate;
            (cell.plate != plate).then_some((n, plate))
        }) {
            let plate2 = state.plates[p2 as usize];
            if plate.motion.dot(plate2.motion) > -0.1 {
                continue; // they aren't opposed enough to be interesting
            }
            if (plate.center_long - plate2.center_long) * (plate.motion.x - plate2.motion.x)
                + (plate.center_lat - plate2.center_lat) * (plate.motion.y - plate2.motion.y)
                > 0.0
            {
                // divergent
            } else {
                // convergent
            }
        }
    }
}
