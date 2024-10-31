use bevy::math::Vec2;
use rand::distributions::Standard;
use rand::prelude::*;
use rand_distr::{Normal, StandardNormal};
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
pub enum CellFeatureKind {
    #[default]
    None,
    Ridge,
    Subduction,
    Mountain,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CellFeature {
    pub kind: CellFeatureKind,
    pub dist: u8,
}

impl CellFeature {
    pub const NONE: Self = Self {
        kind: CellFeatureKind::None,
        dist: 0,
    };
    pub const RIDGE: Self = Self {
        kind: CellFeatureKind::Ridge,
        dist: 0,
    };
    pub const SUBDUCT: Self = Self {
        kind: CellFeatureKind::Subduction,
        dist: 0,
    };
    pub const MOUNTAIN: Self = Self {
        kind: CellFeatureKind::Mountain,
        dist: 0,
    };
}

/// A single cell for the plate tectonics
#[derive(Debug, Default, Clone, Copy)]
pub struct TectonicCell {
    pub plate: u8,
    pub height: f32,
    pub density: u32,
    pub feats: CellFeature,
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
        use std::f32::consts::PI;
        TectonicPlate {
            center_lat: rng.gen_range(-1.0f32..1.0).asin(),
            center_long: rng.gen_range(-PI..=PI),
            height: rng.gen_range(-0.1..=0.1),
            density: rng.gen_range(1000..=2000),
            motion: Vec2::new(rng.sample(StandardNormal), rng.sample(StandardNormal)),
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

/// Quality metrics for a sample terrain
#[derive(Debug, Default, Clone, Copy, PartialEq)]
struct QualityMetrics {
    converge: usize,
    diverge: usize,
    bits: u8,
}

/// Initialize a plate setup without any quality metrics
fn init_terrain_impl<R: Rng + ?Sized>(depth: u8, rng: &mut R) -> TerrainState {
    let noise = Normal::new(0.0, 0.5).unwrap();
    let layer = cdshealpix::nested::get(depth);
    let len = layer.n_hash() as _;
    let nplates = rng.gen_range(10..=12);
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
            use std::f32::consts::*;
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
                density: 0,
                feats: CellFeature::NONE,
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

/// Update a terrain, possibly with some metrics on the quality
fn step_terrain_impl<R: Rng + ?Sized>(
    state: &mut TerrainState,
    rng: &mut R,
    metrics: &mut QualityMetrics,
) {
    use std::f64::consts::*;
    let mountain_spread = Normal::new(0.0, 2.0f32.powi(state.depth as _) * 0.00001).unwrap();
    let mountain_height = Normal::new(0.9, 0.05).unwrap();
    let layer = cdshealpix::nested::get(state.depth);
    debug_assert_eq!(layer.n_hash(), state.cells.len() as u64);
    for i in state.boundaries.iter() {
        let cell = state.cells[i as usize];
        let plate = state.plates[cell.plate as usize];
        let set = neighbors(state.depth, i);
        {
            let cell = &mut state.cells[i as usize];
            if let CellFeature {
                kind: CellFeatureKind::Mountain,
                dist,
            } = cell.feats
            {
                let new = cell.height.mul_add(
                    0.5,
                    (-(dist as f32).max(0.5).powi(2)).exp().mul_add(0.5, 0.5)
                        * rng.sample(mountain_height),
                );
                cell.height = new;
            }
        }
        for &c2 in set {
            let other = state.cells[c2 as usize];
            let p2 = other.plate;
            let plate2 = state.plates[p2 as usize];
            let (lon1, lat1) = layer.center(i);
            if plate.motion.dot(plate2.motion) > -0.05 {
                continue; // they aren't opposed enough to be interesting
            }
            if (plate.center_long - plate2.center_long) * (plate.motion.x - plate2.motion.x)
                + (plate.center_lat - plate2.center_lat) * (plate.motion.y - plate2.motion.y)
                > 0.0
            {
                // divergent
                metrics.diverge += 1;
                metrics.bits |= 1 << (lon1 % TAU / FRAC_PI_4) as u8;
                let cell = &mut state.cells[i as usize];
                cell.feats = CellFeature::RIDGE;
                cell.density /= 20;
                cell.density += 100;
                cell.height = cell.height.mul_add(0.6, 1.0);
            } else {
                // convergent
                if cell.density < other.density {
                    continue;
                }
                metrics.converge += 1;
                metrics.bits |= 1 << (lon1 % TAU / FRAC_PI_4) as u8;
                let cell = &mut state.cells[i as usize];
                cell.feats = CellFeature::SUBDUCT;
                cell.height -= 5.0;
                let (lon2, lat2) = layer.center(c2);
                let pos1 = Vec2::new(lon1 as _, lat1 as _);
                let delta = (Vec2::new(lon2 as _, lat2 as _) - pos1).normalize_or_zero();
                let Vec2 { x, y } = pos1 + plate.motion.normalize_or_zero() * 0.25 + delta * 0.25;
                let new = layer.hash(
                    ((x as f64 + PI) % TAU) - PI,
                    (y as f64).clamp(-FRAC_PI_2, FRAC_PI_2),
                );
                // state.plates[cell.plate as usize].motion += delta * 0.01;
                if new == i {
                    continue;
                }
                let cell = &mut state.cells[new as usize];
                cell.feats = CellFeature::MOUNTAIN;
                cell.height += 1.0;
            }
        }
        for &n in neighbors(state.depth, i) {
            let other = state.cells[n as usize];
            if other.plate != cell.plate {
                continue;
            }
            if let CellFeature {
                kind: CellFeatureKind::Mountain,
                dist,
            } = other.feats
            {
                if cell.feats.kind == CellFeatureKind::Mountain
                    && cell.feats.dist < other.feats.dist
                    && far_neighbors(state.depth, i)
                        .iter()
                        .all(|&h| state.cells[h as usize].feats.kind == CellFeatureKind::None)
                    && rng.sample(mountain_spread).abs() as u8 > dist
                {
                    state.cells[i as usize].feats = CellFeature {
                        kind: CellFeatureKind::Mountain,
                        dist: dist + 1,
                    };
                }
            } else if cell.feats.kind == CellFeatureKind::Mountain
                && rng.gen_ratio(cell.feats.dist as _, 1)
            {
                state.cells[i as usize].feats = CellFeature::NONE;
            }
        }
        let mut dens_sum = 0;
        let mut height_sum = 0.0;
        let mut neigh_count = 0;
        for &n in far_neighbors(state.depth, i) {
            let c2 = state.cells[n as usize];
            if cell.plate != c2.plate {
                continue;
            }
            dens_sum += c2.density;
            height_sum += c2.height;
            neigh_count += 1;
        }
        if neigh_count > 0 {
            let cell = &mut state.cells[i as usize];
            cell.density = cell.density * 9 / 10;
            cell.density += dens_sum / neigh_count;
            cell.height =
                height_sum.mul_add((neigh_count as f32).recip(), rng.gen_range(-0.1..=0.1));
        }
    }
}

pub fn init_terrain<R: Rng + ?Sized>(depth: u8, rng: &mut R) -> TerrainState {
    let scale = 1 << depth;
    loop {
        let mut state = init_terrain_impl(depth, rng);
        let mut metrics = QualityMetrics::default();
        step_terrain_impl(&mut state, rng, &mut metrics);
        bevy::log::info!(?metrics, "sample");
        if metrics.converge / scale > 5
            && metrics.diverge / scale > 5
            && (metrics.converge + metrics.diverge) / scale > 12
            && metrics.bits.count_ones() > 5
        {
            return state;
        }
    }
}

pub fn step_terrain<R: Rng + ?Sized>(state: &mut TerrainState, rng: &mut R) {
    step_terrain_impl(state, rng, &mut QualityMetrics::default());
}
