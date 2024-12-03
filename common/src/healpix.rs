//! Wrapper around [`cdshealpix`] with a few changes:
//! - Caching neighbor lookups
//! - Coordinates use `LonLat` (they have a `f32` representation)
//! - American English spellings of "neighbor"
use crate::coords::LonLat;
use crate::util::MaybeArc;
pub use cdshealpix as cds;
use cdshealpix::compass_point::*;
use cdshealpix::external_edge::ExternalEdge;
pub use cdshealpix::nested;
pub use cdshealpix::{n_hash, nside};
use copyvec::CopyVec;
use quick_cache::sync::Cache;
use std::convert::Infallible;
use std::fmt::{self, Debug, Formatter};
use std::sync::{LazyLock, OnceLock};
use triomphe::Arc;

static NEIGHBORS: [OnceLock<Box<[u64]>>; 29] = [const { OnceLock::new() }; 29];
static SPARSE_NEIGHBORS: [LazyLock<Cache<u64, CopyVec<u64, 8>>>; 23] =
    [const { LazyLock::new(|| Cache::new(65536)) }; 23];

#[allow(clippy::type_complexity)]
static FAR_NEIGHBORS: [OnceLock<Box<[OnceLock<Box<[u64]>>]>>; 29] = [const { OnceLock::new() }; 29]; // TODO: precompute and save to a file
static SPARSE_FAR_NEIGHBORS: [LazyLock<Cache<u64, Arc<[u64]>>>; 23] =
    [const { LazyLock::new(|| Cache::new(65536)) }; 23];

fn neighbors_list(depth: u8) -> &'static [u64] {
    NEIGHBORS[depth as usize].get_or_init(|| {
        let layer = nested::get(depth);
        let len = layer.n_hash();
        let mut data = Vec::with_capacity(len as usize * 8);
        for i in 0..len {
            layer.append_bulk_neighbours(i, &mut data);
            let new_len = (i as usize + 1) * 8;
            debug_assert!(
                data.len() <= new_len,
                "more than eight neighbors for cell {i}, found {}",
                data.len() - new_len + 8
            );
            data.resize(new_len, u64::MAX);
        }
        data.into_boxed_slice()
    })
}
/// Neighbors for a given cell.
/// The third parameter determines if we want to initialize a full array or use a LRU cache. The former is better if all are needed at once, but the latter is better if only a few are needed.
pub fn neighbors(depth: u8, hash: u64, full: bool) -> CopyVec<u64, 8> {
    if full || depth < 6 {
        let max_slice = &neighbors_list(depth)[(hash as usize * 8)..(hash as usize * 8 + 8)];
        let slice = max_slice
            .split_once(|x| *x == u64::MAX)
            .map_or(max_slice, |x| x.0);
        let mut vec = slice.try_into().map_or_else(
            |_| CopyVec::try_from_iter(slice.iter().copied()).unwrap(),
            CopyVec::from_array,
        );
        vec.retain(|i| *i != u64::MAX);
        vec
    } else if let Some(max_slice) = NEIGHBORS[depth as usize].get() {
        let slice = max_slice
            .split_once(|x| *x == u64::MAX)
            .map_or(&**max_slice, |x| x.0);
        let mut vec = slice.try_into().map_or_else(
            |_| CopyVec::try_from_iter(slice.iter().copied()).unwrap(),
            CopyVec::from_array,
        );
        vec.retain(|i| *i != u64::MAX);
        vec
    } else {
        let cache = &SPARSE_NEIGHBORS[depth as usize - 6];
        let Ok(res) = cache.get_or_insert_with(&hash, || {
            let map = nested::neighbours(depth, hash, false);
            Ok::<_, Infallible>(
                CopyVec::try_from_iter(
                    (0..9).filter_map(|i| map.get(MainWind::from_index(i)).copied()),
                )
                .unwrap(),
            )
        });
        res
    }
}
/// Cells nearby a given cell. The number here scales with depth.
pub fn far_neighbors(depth: u8, hash: u64, full: bool) -> MaybeArc<'static, [u64]> {
    if full || depth < 6 {
        let res = FAR_NEIGHBORS[depth as usize]
            .get_or_init(|| vec![OnceLock::new(); n_hash(depth) as usize].into_boxed_slice())
            [hash as usize]
            .get_or_init(|| {
                let mut set = Vec::new();
                let mut edge = vec![hash];
                for _ in 0..((1 << depth.saturating_sub(3)) * 3 / 8) {
                    let end = set.len();
                    set.append(&mut edge);
                    for &i in &set[end..] {
                        edge.extend(
                            neighbors(depth, i, true)
                                .iter()
                                .copied()
                                .filter(|e| !set.contains(e)),
                        );
                    }
                }
                set.sort_unstable();
                set.dedup();
                set.into_boxed_slice()
            });
        MaybeArc::Borrowed(res)
    } else {
        if let Some(neighbors) = FAR_NEIGHBORS[depth as usize].get() {
            if let Some(slice) = neighbors[hash as usize].get() {
                return MaybeArc::Borrowed(slice);
            }
        }
        let Ok(res) = SPARSE_FAR_NEIGHBORS[depth as usize - 6].get_or_insert_with(&hash, || {
            let mut set = Vec::new();
            let mut edge = vec![hash];
            for _ in 0..((1 << depth.saturating_sub(3)) * 3 / 8) {
                let end = set.len();
                set.append(&mut edge);
                for &i in &set[end..] {
                    edge.extend(
                        neighbors(depth, i, true)
                            .iter()
                            .copied()
                            .filter(|e| !set.contains(e)),
                    );
                }
            }
            set.sort_unstable();
            set.dedup();
            Ok::<_, Infallible>(Arc::from(set))
        });
        MaybeArc::Owned(res)
    }
}

/// A wrapper around [`nested::Layer`] to use my APIs.
#[derive(Clone, Copy)]
pub struct Layer(&'static cdshealpix::nested::Layer);
impl Debug for Layer {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Layer")
            .field("depth", &self.0.depth())
            .finish_non_exhaustive()
    }
}
impl Layer {
    #[inline(always)]
    pub fn new(depth: u8) -> Self {
        Self(nested::get(depth))
    }
    pub fn depth(&self) -> u8 {
        self.0.depth()
    }
    pub fn n_hash(&self) -> u64 {
        self.0.n_hash()
    }
    pub fn inner(&self) -> &'static nested::Layer {
        self.0
    }
    pub fn hash(&self, coords: LonLat) -> u64 {
        let (lon, lat) = coords.as_f64();
        self.0
            .hash_checked(lon, lat)
            .unwrap_or_else(|msg| panic!("Hash of {coords:?} failed with message {msg:?}"))
    }
    pub fn hash_checked(&self, coords: LonLat) -> Result<u64, String> {
        let (lon, lat) = coords.as_f64();
        self.0.hash_checked(lon, lat)
    }
    pub fn hash_with_dxdy(&self, coords: LonLat) -> (u64, f32, f32) {
        let (lon, lat) = coords.as_f64();
        let (hash, dx, dy) = self.0.hash_with_dxdy(lon, lat);
        (hash, dx as _, dy as _)
    }
    pub fn center(&self, hash: u64) -> LonLat {
        self.0.center(hash).into()
    }
    pub fn vertex(&self, hash: u64, dir: Cardinal) -> LonLat {
        self.0.vertex(hash, dir).into()
    }
    pub fn vertices(&self, hash: u64) -> [LonLat; 4] {
        self.0.vertices(hash).map(LonLat::from)
    }
    #[inline(always)]
    pub fn neighbor(&self, hash: u64, direction: MainWind) -> Option<u64> {
        self.0.neighbour(hash, direction)
    }
    #[inline(always)]
    pub fn neighbors(&self, hash: u64, include_center: bool) -> MainWindMap<u64> {
        self.0.neighbours(hash, include_center)
    }
    #[inline(always)]
    pub fn neighbors_slice(&self, hash: u64, full: bool) -> CopyVec<u64, 8> {
        neighbors(self.depth(), hash, full)
    }
    #[inline(always)]
    pub fn append_bulk_neighbours(&self, hash: u64, dest: &mut Vec<u64>) {
        self.0.append_bulk_neighbours(hash, dest);
    }
    #[inline(always)]
    pub fn internal_edge(hash: u64, delta: u8) -> Box<[u64]> {
        nested::Layer::internal_edge(hash, delta)
    }
    #[inline(always)]
    pub fn internal_edge_sorted(hash: u64, delta: u8) -> Box<[u64]> {
        nested::Layer::internal_edge_sorted(hash, delta).into_boxed_slice()
    }
    #[inline(always)]
    pub fn external_edge_struct(&self, hash: u64, delta: u8) -> ExternalEdge {
        self.0.external_edge_struct(hash, delta)
    }
    #[inline(always)]
    pub fn external_edge(&self, hash: u64, delta: u8) -> Box<[u64]> {
        self.0.external_edge(hash, delta)
    }
    #[inline(always)]
    pub fn external_edge_sorted(&self, hash: u64, delta: u8) -> Box<[u64]> {
        self.0.external_edge_sorted(hash, delta)
    }
    #[inline(always)]
    pub fn append_external_edge(&self, hash: u64, delta: u8, dest: &mut Vec<u64>) {
        self.0.append_external_edge(hash, delta, dest);
    }
    #[inline(always)]
    pub fn append_external_edge_sorted(&self, hash: u64, delta: u8, dest: &mut Vec<u64>) {
        self.0.append_external_edge_sorted(hash, delta, dest);
    }
    pub fn bilinear_interpolation(&self, coords: LonLat) -> [(u64, f32); 4] {
        let (lon, lat) = coords.as_f64();
        self.0
            .bilinear_interpolation(lon, lat)
            .map(|(c, w)| (c, w as _))
    }
}
