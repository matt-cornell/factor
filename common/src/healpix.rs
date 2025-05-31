//! Wrapper around [`healpix`] with caches for neighbor lookups
use crate::util::MaybeArc;
use copyvec::CopyVec;
use quick_cache::sync::Cache;
use std::convert::Infallible;
use std::sync::{LazyLock, OnceLock};
use triomphe::Arc;

pub use healpix::*;

static NEIGHBORS: [OnceLock<Box<[u64]>>; 29] = [const { OnceLock::new() }; 29];
static SPARSE_NEIGHBORS: [LazyLock<Cache<u64, CopyVec<u64, 8>>>; 23] =
    [const { LazyLock::new(|| Cache::new(65536)) }; 23];

static FAR_NEIGHBORS: [OnceLock<Box<[OnceLock<Box<[u64]>>]>>; 29] = [const { OnceLock::new() }; 29]; // TODO: precompute and save to a file
static SPARSE_FAR_NEIGHBORS: [LazyLock<Cache<u64, Arc<[u64]>>>; 23] =
    [const { LazyLock::new(|| Cache::new(65536)) }; 23];

fn neighbors_list(depth: u8) -> &'static [u64] {
    NEIGHBORS[depth as usize].get_or_init(|| {
        let layer = healpix::get(depth);
        let len = layer.n_hash();
        let mut data = Vec::with_capacity(len as usize * 8);
        for i in 0..len {
            layer.append_bulk_neighbors(i, &mut data);
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
            let map = healpix::get(depth).neighbors(hash);
            Ok::<_, Infallible>(CopyVec::try_from_iter(map.into_values()).unwrap())
        });
        res
    }
}
/// Cells nearby a given cell. The number here scales with depth.
pub fn far_neighbors(depth: u8, hash: u64, full: bool) -> MaybeArc<'static, [u64]> {
    if full || depth < 6 {
        let res = FAR_NEIGHBORS[depth as usize].get_or_init(|| {
            vec![OnceLock::new(); healpix::checked::n_hash(depth) as usize].into_boxed_slice()
        })[hash as usize]
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
