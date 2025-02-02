use crate::config::WorldConfig;
use crate::tables::{NoiseLocation, CHUNKS, HIGH_GRAD_NOISE, HIGH_VALUE_NOISE, TERRAIN};
use crate::utils::database::Database;
use crate::utils::mesh::*;
use bevy::prelude::*;
use bevy::utils::{Entry, HashMap};
use crossbeam_channel::Receiver;
use factor_common::coords::{get_absolute, get_relative, LonLat};
use factor_common::data::{ChunkId, PlayerId};
use factor_common::{healpix, PLANET_RADIUS};
use itertools::{EitherOrBoth, Itertools};
use ordered_float::OrderedFloat;
use priority_queue::PriorityQueue;
use rand::prelude::*;
use redb::ReadableTable as _;
use serde::{Deserialize, Serialize};
use std::io;
use std::num::NonZeroUsize;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use triomphe::Arc;

/// The server and client need to have separate chunks, this component marks the server's
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Component)]
pub struct ServerChunk;

/// Chunk interest, at least that the server knows about. The client may have its own ideas but we need our own copy to track changes anyways.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash, Component)]
pub struct ChunkInterest {
    pub chunks: Vec<u64>,
}

#[derive(Debug, Clone, Copy, PartialEq, Component)]
pub struct ChunkCorners {
    pub corners: [Vec2; 4],
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ChunkData {
    pub surface: MeshData,
    pub corners: [Vec2; 4],
}

#[derive(Debug, Clone, Copy, PartialEq, Event)]
pub struct ChunkRequest {
    pub id: u64,
}
impl ChunkRequest {
    pub const fn new(id: u64) -> Self {
        Self { id }
    }
}

/// Unload a chunk. This is a buffered event, and is mostly used internally to this file.
///
/// If this event is sent, the chunk is unloaded. It doesn't check if someone else is still using it.
#[derive(Debug, Clone, Copy, PartialEq, Event)]
pub struct UnloadChunk {
    pub id: u64,
}
impl UnloadChunk {
    pub const fn new(id: u64) -> Self {
        Self { id }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Event)]
pub struct ChunkLoaded {
    pub id: u64,
    pub entity: Entity,
}

/// A player's chunk interest has changed. Unlike most events, this one is going to be buffered because it's more efficient to handle these changes all at once.
#[derive(Debug, Clone, Event)]
pub struct InterestChanged {
    /// The player whose interest changed.
    pub player: PlayerId,
    /// Chunks, sorted from most to least important.
    pub new: Vec<u64>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ChunkLoadingState {
    Queued(ChunkPriority),
    Loaded(Entity),
}

#[derive(Debug, Clone)]

pub struct ChunkPriority {
    pub sum: f32,
    pub contributors: HashMap<PlayerId, f32>,
}
impl PartialEq for ChunkPriority {
    fn eq(&self, other: &Self) -> bool {
        self.sum == other.sum
    }
}
impl PartialOrd for ChunkPriority {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Eq for ChunkPriority {}
impl Ord for ChunkPriority {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.sum.total_cmp(&other.sum)
    }
}

/// A map of chunks to how many players want them loaded.
#[derive(Debug, Default, Clone, Resource)]
pub struct LoadedChunks {
    pub chunks: HashMap<u64, (ChunkLoadingState, NonZeroUsize)>,
}

fn add_interest(
    player: PlayerId,
    chunk: u64,
    chunks: &mut LoadedChunks,
    weight: &mut f32,
    handle: &mut ChunkloaderHandle,
) {
    match chunks.chunks.entry(chunk) {
        Entry::Occupied(e) => {
            if let (ChunkLoadingState::Queued(priority), count) = e.into_mut() {
                priority.sum += *weight;
                if let Some(old) = priority.contributors.insert(player, *weight) {
                    priority.sum -= old;
                } else {
                    *count = unsafe { NonZeroUsize::new_unchecked(count.get() + 1) };
                }
                // if this is none, then the chunk is already in the ringbuffer and shouldn't be counted
                if handle
                    .queue
                    .change_priority(&chunk, OrderedFloat(priority.sum))
                    .is_some()
                {
                    *weight *= 0.5;
                }
            }
        }
        Entry::Vacant(e) => {
            e.insert((
                ChunkLoadingState::Queued(ChunkPriority {
                    sum: *weight,
                    contributors: [(player, *weight)].into(),
                }),
                unsafe { NonZeroUsize::new_unchecked(1) },
            ));
            let opt = handle.queue.push(chunk, OrderedFloat(*weight));
            debug_assert_eq!(opt, None);
            *weight *= 0.5;
        }
    }
}

fn rem_interest(
    player: PlayerId,
    chunk: u64,
    chunks: &mut LoadedChunks,
    handle: &mut ChunkloaderHandle,
) -> bool {
    let Some((state, count)) = chunks.chunks.get_mut(&chunk) else {
        error!(
            chunk,
            %player,
            "Lost interest in a chunk that already had no interest"
        );
        return false;
    };
    if let Some(new_count) = NonZeroUsize::new(count.get() - 1) {
        *count = new_count;
        if let ChunkLoadingState::Queued(priority) = state {
            if let Some(weight) = priority.contributors.remove(&player) {
                priority.sum -= weight;
                handle
                    .queue
                    .change_priority(&chunk, OrderedFloat(priority.sum));
            } else {
                error!(chunk, %player, "Stopped contributing to a chunk that this player already wasn't contributing to");
            }
        }
        false
    } else {
        true
    }
}

fn update_interest(
    change: &InterestChanged,
    interest: &mut ChunkInterest,
    chunks: &mut LoadedChunks,
    handle: &mut ChunkloaderHandle,
    prune: &mut tinyset::SetU64,
) {
    let mut weight = 0.5;
    for pair in change.new.iter().zip_longest(&mut interest.chunks) {
        match pair {
            EitherOrBoth::Both(l, r) => {
                if *l == *r {
                    continue;
                }
                prune.remove(*l);
                if rem_interest(change.player, *r, chunks, handle) {
                    prune.insert(*r);
                }
                add_interest(change.player, *l, chunks, &mut weight, handle);
                *r = *l;
            }
            EitherOrBoth::Left(l) => {
                prune.remove(*l);
                add_interest(change.player, *l, chunks, &mut weight, handle)
            }
            EitherOrBoth::Right(r) => {
                if rem_interest(change.player, *r, chunks, handle) {
                    prune.remove(*r);
                }
            }
        }
    }
    if interest.chunks.len() < change.new.len() {
        interest
            .chunks
            .extend_from_slice(&change.new[interest.chunks.len()..]);
    } else {
        interest.chunks.truncate(change.new.len());
    }
}

pub fn handle_interests(
    mut chunks: ResMut<LoadedChunks>,
    mut changes: EventReader<InterestChanged>,
    mut query: Query<(&PlayerId, &mut ChunkInterest)>,
    mut handle: ResMut<ChunkloaderHandle>,
    mut unload: EventWriter<UnloadChunk>,
) {
    let mut prune = tinyset::SetU64::new();
    match changes.len() {
        0 => {}
        1 => {
            let change = changes.read().next().unwrap();
            let Some(mut interest) = query
                .iter_mut()
                .find_map(|(p, i)| (*p == change.player).then_some(i))
            else {
                warn!(player = %change.player, "Interests changed for non-existent player");
                return;
            };
            update_interest(change, &mut interest, &mut chunks, &mut handle, &mut prune);
        }
        _ => {
            let mut lookup = query
                .iter_mut()
                .map(|(p, i)| (*p, i))
                .collect::<HashMap<_, _>>();

            for change in changes.read() {
                let Some(interest) = lookup.get_mut(&change.player) else {
                    warn!(player = %change.player, "Interests changed for non-existent player");
                    continue;
                };
                update_interest(change, interest, &mut chunks, &mut handle, &mut prune);
            }
        }
    }
    for chunk in prune {
        handle.queue.remove(&chunk);
        if let Some((ChunkLoadingState::Loaded(_), _)) = chunks.chunks.remove(&chunk) {
            unload.send(UnloadChunk::new(chunk));
        }
    }
}

pub fn unload_chunks(
    mut commands: Commands,
    query: Query<(Entity, &ChunkId), With<ServerChunk>>,
    mut to_unload: EventReader<UnloadChunk>,
) {
    let mut unloaded = to_unload.read().map(|u| u.id).collect::<tinyset::SetU64>();
    if unloaded.is_empty() {
        return;
    }
    for (entity, &chunk) in query.iter() {
        if unloaded.remove(chunk.0) {
            commands.entity(entity).despawn();
            if unloaded.is_empty() {
                return;
            }
        }
    }
}

const BUF_SIZE: usize = 4;

struct RingBufferBody {
    cancel: AtomicBool,
    head: AtomicUsize,
    tail: AtomicUsize,
    body: [AtomicU64; BUF_SIZE],
}
impl RingBufferBody {
    const fn new() -> Self {
        Self {
            cancel: AtomicBool::new(false),
            head: AtomicUsize::new(0),
            tail: AtomicUsize::new(0),
            body: [const { AtomicU64::new(0) }; BUF_SIZE],
        }
    }
    /// Pop an element from the buffer.
    fn pop(&self) -> Option<u64> {
        loop {
            let head = self.head.load(Ordering::Acquire);
            let tail = self.tail.load(Ordering::Acquire);
            if head == tail {
                return None;
            }
            let new_start = (head + 1) % BUF_SIZE;
            if self
                .head
                .compare_exchange_weak(head, new_start, Ordering::Release, Ordering::Relaxed)
                .is_err()
            {
                continue;
            }
            return Some(self.body[head].load(Ordering::Acquire));
        }
    }
    /// Fill with an iterator, this can cause logic errors if there are multiple writers.
    fn fill(&self, iter: &mut dyn Iterator<Item = u64>) -> usize {
        let mut count = 0;
        let mut tail = self.tail.load(Ordering::Acquire);
        loop {
            let head = self.head.load(Ordering::Acquire);
            let new_tail = (tail + 1) % BUF_SIZE;
            if new_tail == head {
                break;
            }
            let Some(value) = iter.next() else {
                break;
            };
            self.body[tail].store(value, Ordering::Release);
            self.tail.store(new_tail, Ordering::Release);
            tail = new_tail;
            count += 1;
        }
        count
    }
}
unsafe impl Sync for RingBufferBody {}

#[derive(Resource)]
pub struct ChunkloaderHandle {
    finished: Receiver<(u64, ChunkData)>,
    shared: Arc<RingBufferBody>,
    queue: PriorityQueue<u64, OrderedFloat<f32>>,
}
impl ChunkloaderHandle {
    pub fn spawn(config: &WorldConfig, db: &Database) -> Self {
        let (finished_tx, finished_rx) = crossbeam_channel::bounded(8);
        let shared = Arc::new(RingBufferBody::new());
        for i in 0..std::thread::available_parallelism().map_or(1, |n| n.get()) {
            let queue = Arc::clone(&shared);
            let config = config.clone();
            let db = db.clone();
            let finished_tx = finished_tx.clone();
            let res = std::thread::Builder::new()
                .name(format!("chunkload-{i}"))
                .spawn(move || {
                    while !queue.cancel.load(Ordering::Acquire) {
                        if let Some(req) = queue.pop() {
                            match load_chunk(&config, &db, req) {
                                Ok(data) => {
                                    if finished_tx.send((req, data)).is_err() {
                                        break;
                                    }
                                }
                                Err(err) => {
                                    error!(id = req, %err, "Error loading chunk");
                                }
                            }
                        } else {
                            std::thread::sleep(std::time::Duration::from_millis(10));
                        }
                    }
                });
            if let Err(err) = res {
                error!(%err, "Failed to spawn thread");
            }
        }
        Self {
            finished: finished_rx,
            shared,
            queue: PriorityQueue::new(),
        }
    }
    pub fn finished(&self) -> impl Iterator<Item = (u64, ChunkData)> + '_ {
        self.finished.try_iter()
    }
    pub fn cancel(&self) {
        self.shared.cancel.store(true, Ordering::Release);
    }
}
impl Drop for ChunkloaderHandle {
    fn drop(&mut self) {
        self.cancel();
    }
}

pub fn loader_interface(
    mut commands: Commands,
    mut loaded_chunks: ResMut<LoadedChunks>,
    mut loader: ResMut<ChunkloaderHandle>,
) {
    let loader = &mut *loader;
    loader
        .shared
        .fill(&mut std::iter::from_fn(|| loader.queue.pop().map(|v| v.0)));
    for (id, data) in loader.finished() {
        let entity = commands
            .spawn((
                ChunkId(id),
                data.surface,
                ChunkCorners {
                    corners: data.corners,
                },
                ServerChunk,
            ))
            .id();
        if let Some((tracked, _)) = loaded_chunks.chunks.get_mut(&id) {
            if let ChunkLoadingState::Loaded(old) = tracked {
                error!(id, %old, new = %entity, "Duplicate loaded chunk");
            }
            *tracked = ChunkLoadingState::Loaded(entity);
            commands.trigger(ChunkLoaded { id, entity });
        } else {
            error!(id, %entity, "Loaded chunk that's not in interest");
            commands.entity(entity).despawn();
        }
    }
}

fn load_chunk(config: &WorldConfig, db: &Database, id: u64) -> Result<ChunkData, redb::Error> {
    {
        let txn = db.begin_read()?;
        match txn.open_table(CHUNKS) {
            Ok(table) => {
                if let Some(chunk) = table.get(id)? {
                    if let Some(data) = chunk.value() {
                        drop(table);
                        txn.close()?;
                        return Ok(data);
                    }
                }
                drop(table);
                txn.close()?;
            }
            Err(redb::TableError::TableDoesNotExist(_)) => {}
            Err(err) => Err(err)?,
        }
    }
    let (surface, corners) = setup_chunk(config, id, db)?;
    let opt_data = Some(ChunkData { surface, corners });

    let txn = db.begin_write()?;
    let mut table = txn.open_table(CHUNKS)?;
    table.insert(id, &opt_data)?;
    drop(table);
    txn.commit()?;

    Ok(opt_data.unwrap())
}

fn get_rng(mut seed: [u8; 32], layer: u8, hash: u64) -> rand_xoshiro::Xoshiro256PlusPlus {
    for (n, seed) in seed.iter_mut().enumerate() {
        *seed ^= layer.rotate_left(n as _);
    }
    let hash_bytes = hash.to_le_bytes();
    for (seed, hash) in seed[1..].iter_mut().zip(hash_bytes) {
        *seed ^= hash;
    }
    SeedableRng::from_seed(seed)
}

fn get_height(config: &WorldConfig, coords: LonLat, db: &Database) -> Result<f32, redb::Error> {
    let mut height = {
        let txn = db.begin_read()?;
        let terrain = txn.open_table(TERRAIN)?;
        let layer = healpix::Layer::new(config.climate.depth);
        let mut sum = 0.0;
        let mut scale = 0.0;
        for (n, w) in layer.bilinear_interpolation(coords) {
            scale += w;
            let h = terrain.get(n)?.ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("Missing climate data for chunk {n}"),
                )
            })?;
            let cell = h.value();
            sum += cell.height * w;
        }
        drop(terrain);
        txn.close()?;
        sum / scale
    };
    let mut data = either::Left(db.begin_read()?);
    for (n, noise) in config.noise.local.iter().enumerate() {
        let mut coords = coords;
        coords.lon -= noise.shift as f64;
        let layer = healpix::Layer::new(noise.depth);
        let mut sum = 0.0;
        let mut scale = 0.0;
        let grad_scale = ((1 << (noise.depth * 2)) as f32).recip() * 0.5;
        if noise.gradient {
            for (cell, w) in layer.bilinear_interpolation(coords) {
                let c = layer.center(cell);
                let loc = NoiseLocation {
                    layer: n as _,
                    cell,
                };
                let val = match data {
                    either::Left(txn) => {
                        let opt = match txn.open_table(HIGH_GRAD_NOISE) {
                            Ok(table) => table.get(loc)?.as_ref().map(redb::AccessGuard::value),
                            Err(redb::TableError::TableDoesNotExist(_)) => None,
                            Err(err) => Err(err)?,
                        };
                        if let Some(v) = opt {
                            data = either::Left(txn);
                            v
                        } else {
                            txn.close()?;
                            let txn = db.begin_write()?;
                            let mut grad = txn.open_table(HIGH_GRAD_NOISE)?;
                            let opt = grad.get(loc)?.as_ref().map(redb::AccessGuard::value);
                            let val = if let Some(v) = opt {
                                v
                            } else {
                                let mut rng = get_rng(config.seed.unwrap(), loc.layer, loc.cell);
                                let v = rng.sample(rand_distr::UnitCircle);
                                grad.insert(loc, v)?;
                                v
                            };
                            drop(grad);
                            data = either::Right(txn);
                            val
                        }
                    }
                    either::Right(txn) => {
                        let mut grad = txn.open_table(HIGH_GRAD_NOISE)?;
                        let opt = grad.get(loc)?.as_ref().map(redb::AccessGuard::value);
                        let val = if let Some(v) = opt {
                            v
                        } else {
                            let mut rng = get_rng(config.seed.unwrap(), loc.layer, loc.cell);
                            let v = rng.sample(rand_distr::UnitCircle);
                            grad.insert(loc, v)?;
                            v
                        };
                        drop(grad);
                        data = either::Right(txn);
                        val
                    }
                };
                sum += Vec2::from(val)
                    .dot(get_relative(c, coords).as_vec2())
                    .mul_add(grad_scale, 0.5)
                    * w;
                scale += w;
            }
        } else {
            for (cell, w) in layer.bilinear_interpolation(coords) {
                let loc = NoiseLocation {
                    layer: n as _,
                    cell,
                };
                let val = match data {
                    either::Left(txn) => {
                        let opt = match txn.open_table(HIGH_VALUE_NOISE) {
                            Ok(table) => table.get(loc)?.as_ref().map(redb::AccessGuard::value),
                            Err(redb::TableError::TableDoesNotExist(_)) => None,
                            Err(err) => Err(err)?,
                        };
                        if let Some(v) = opt {
                            data = either::Left(txn);
                            v
                        } else {
                            txn.close()?;
                            let txn = db.begin_write()?;
                            let mut value = txn.open_table(HIGH_VALUE_NOISE)?;
                            let opt = value.get(loc)?.as_ref().map(redb::AccessGuard::value);
                            let val = if let Some(v) = opt {
                                v
                            } else {
                                let mut rng = get_rng(config.seed.unwrap(), loc.layer, loc.cell);
                                let v = rng.gen();
                                value.insert(loc, v)?;
                                v
                            };
                            drop(value);
                            data = either::Right(txn);
                            val
                        }
                    }
                    either::Right(txn) => {
                        let mut value = txn.open_table(HIGH_VALUE_NOISE)?;
                        let opt = value.get(loc)?.as_ref().map(redb::AccessGuard::value);
                        let val = if let Some(v) = opt {
                            v
                        } else {
                            let mut rng = get_rng(config.seed.unwrap(), loc.layer, loc.cell);
                            let v = rng.gen();
                            value.insert(loc, v)?;
                            v
                        };
                        drop(value);
                        data = either::Right(txn);
                        val
                    }
                };
                sum += w;
                scale += val * w;
            }
        }
        height += sum / scale;
    }
    match data {
        either::Left(txn) => {
            txn.close()?;
        }
        either::Right(mut txn) => {
            txn.set_durability(redb::Durability::None);
            txn.commit()?;
        }
    }
    Ok(height * 10000.0)
}

fn setup_chunk(
    config: &WorldConfig,
    hash: u64,
    db: &Database,
) -> Result<(MeshData, [Vec2; 4]), redb::Error> {
    let center = healpix::Layer::new(12).center(hash >> 8);
    let chunk_corners = healpix::Layer::new(16)
        .vertices(hash)
        .map(|abs| (get_relative(center, abs) * PLANET_RADIUS).as_vec2()); // SENW
    let mesh = try_mesh_quad(chunk_corners, 64, move |MeshPoint { abs, .. }| {
        get_height(
            config,
            get_absolute(center, abs.as_dvec2() / PLANET_RADIUS),
            db,
        )
    });
    mesh.map(|mesh| (mesh, chunk_corners))
}

fn barycentric(point: Vec2, tri: [Vec2; 3]) -> [f32; 3] {
    let [a, b, c] = tri;
    let x1 = a - c;
    let x2 = b - c;
    let r = point - c;

    let &[u, v] = (Mat2::from_cols(x1, x2).inverse() * r).as_ref();
    let w = 1.0 - u - v;
    [u, v, w]
}
pub fn chunk_height(p: Vec2, mesh: &MeshData) -> Option<f32> {
    let mut res = f32::NEG_INFINITY;
    let mut found = false;
    for &tri in &mesh.triangles {
        let pts = tri.map(|i| mesh.vertices[i as usize]);
        let bary = barycentric(p, pts.map(Vec3::xz));
        if bary.iter().any(|i| *i < -0.0001) {
            continue;
        }
        res = res.max(pts.iter().map(|v| v.y).zip(bary).map(|(a, b)| a * b).sum());
        found = true;
    }
    if found {
        Some(res)
    } else {
        None
    }
}
