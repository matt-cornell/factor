pub use cdshealpix::*;
use std::sync::OnceLock;

static NEIGHBORS: [OnceLock<Box<[u64]>>; 29] = [const { OnceLock::new() }; 29];
#[allow(clippy::type_complexity)]
static FAR_NEIGHBORS: [OnceLock<Box<[OnceLock<Box<[u64]>>]>>; 29] = [const { OnceLock::new() }; 29]; // TODO: precompute and save to a file
pub fn neighbors_list(depth: u8) -> &'static [u64] {
    NEIGHBORS[depth as usize].get_or_init(|| {
        let layer = nested::get(depth);
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
    FAR_NEIGHBORS[depth as usize]
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
