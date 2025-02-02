use crate::config::WorldConfig;
use crate::tables::{NoiseLocation, CHUNKS, HIGH_GRAD_NOISE, HIGH_VALUE_NOISE, TERRAIN};
use crate::utils::ctx_pool::CtxPool;
use crate::utils::database::Database;
use crate::utils::mesh::*;
use bevy::prelude::*;
use bevy::utils::HashMap;
use crossbeam_channel::Receiver;
use factor_common::coords::{get_absolute, get_relative, LonLat};
use factor_common::data::{ChunkId, ChunkInterest, PlayerId};
use factor_common::{healpix, PLANET_RADIUS};
use priority_queue::PriorityQueue;
use rand::prelude::*;
use redb::ReadableTable as _;
use serde::{Deserialize, Serialize};
use std::cell::UnsafeCell;
use std::io;
use std::num::NonZeroUsize;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

/// The server and client need to have separate chunks, this component marks the server's
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Component)]
pub struct ServerChunk;

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
    /// If we're in singleplayer mode, entities are shared and we don't need to update our copy of the interest.
    pub needs_update: bool,
    pub added: tinyset::SetU64,
    pub removed: tinyset::SetU64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ChunkLoadingState {
    Queued,
    Loading,
    Loaded(Entity),
}

/// A map of chunks to how many players want them loaded.
#[derive(Debug, Default, Clone, Resource)]
pub struct LoadedChunks {
    pub chunks: HashMap<u64, (ChunkLoadingState, NonZeroUsize)>,
}

pub fn handle_interests(
    mut commands: Commands,
    mut chunks: ResMut<LoadedChunks>,
    mut changes: EventReader<InterestChanged>,
    mut query: Query<(&PlayerId, &mut ChunkInterest)>,
    mut load: EventWriter<ChunkRequest>,
    mut unload: EventWriter<UnloadChunk>,
) {
    match changes.len() {
        0 => {}
        1 => {
            let change = changes.read().next().unwrap();
            if change.added.is_empty() && change.removed.is_empty() {
                warn!("No chunks changed in event");
                return;
            }
            let chunks = &mut chunks.chunks;
            if change.needs_update {
                let Some(mut interest) = query
                    .iter_mut()
                    .find_map(|(player, interest)| (*player == change.player).then_some(interest))
                else {
                    warn!(player = %change.player, "Interests changed for non-existent player");
                    return;
                };
                let set = &mut interest.chunks;
                for chunk in change.removed.iter() {
                    if !set.remove(chunk) {
                        warn!(player = %change.player, chunk, "Attempted to remove a chunk that wasn't already in the interest");
                    }
                    if let Some((_, count)) = chunks.get_mut(&chunk) {
                        if count.get() == 1 {
                            chunks.remove(&chunk);
                            unload.send(UnloadChunk::new(chunk));
                        } else {
                            // SAFETY: we already checked this
                            *count = unsafe { NonZeroUsize::new_unchecked(count.get() - 1) };
                        }
                    } else {
                        warn!(chunk, "Attempted to remove a chunk from global interest that wasn't already in the interest");
                        unload.send(UnloadChunk::new(chunk));
                    }
                }
                for chunk in change.added.iter() {
                    if !set.insert(chunk) {
                        warn!(player = %change.player, chunk, "Attempted to add a chunk that was already in the interest");
                    }
                    chunks
                        .entry(chunk)
                        .and_modify(|(_, count)| {
                            // SAFETY: the count only increases here
                            *count = unsafe { NonZeroUsize::new_unchecked(count.get() + 1) };
                        })
                        .or_insert_with(|| {
                            load.send(ChunkRequest::new(chunk));
                            // SAFETY: 1 is not 0
                            (ChunkLoadingState::Queued, unsafe {
                                NonZeroUsize::new_unchecked(1)
                            })
                        });
                }
            } else {
                for chunk in change.removed.iter() {
                    if let Some((_, count)) = chunks.get_mut(&chunk) {
                        if count.get() == 1 {
                            chunks.remove(&chunk);
                            unload.send(UnloadChunk::new(chunk));
                        } else {
                            // SAFETY: we already checked this
                            *count = unsafe { NonZeroUsize::new_unchecked(count.get() - 1) };
                        }
                    } else {
                        warn!(chunk, "Attempted to remove a chunk from global interest that wasn't already in the interest");
                        unload.send(UnloadChunk::new(chunk));
                    }
                }
                for chunk in change.added.iter() {
                    chunks
                        .entry(chunk)
                        .and_modify(|(_, count)| {
                            // SAFETY: the count only increases here
                            *count = unsafe { NonZeroUsize::new_unchecked(count.get() + 1) };
                        })
                        .or_insert_with(|| {
                            load.send(ChunkRequest::new(chunk));
                            // SAFETY: 1 is not 0
                            (ChunkLoadingState::Queued, unsafe {
                                NonZeroUsize::new_unchecked(1)
                            })
                        });
                }
            }
        }
        _ => {
            let mut lookup = query
                .iter_mut()
                .map(|(p, i)| (*p, i))
                .collect::<HashMap<_, _>>();
            for change in changes.read() {
                if change.added.is_empty() && change.removed.is_empty() {
                    warn!("No chunks changed in event");
                    continue;
                }
                let chunks = &mut chunks.chunks;
                if change.needs_update {
                    let Some(interest) = lookup.get_mut(&change.player) else {
                        warn!(player = %change.player, "Interests changed for non-existent player");
                        return;
                    };
                    let set = &mut interest.chunks;
                    for chunk in change.removed.iter() {
                        if !set.remove(chunk) {
                            warn!(player = %change.player, chunk, "Attempted to remove a chunk that wasn't already in the interest");
                        }
                        if let Some((_, count)) = chunks.get_mut(&chunk) {
                            if count.get() == 1 {
                                chunks.remove(&chunk);
                                unload.send(UnloadChunk::new(chunk));
                            } else {
                                // SAFETY: we already checked this
                                *count = unsafe { NonZeroUsize::new_unchecked(count.get() - 1) };
                            }
                        } else {
                            warn!(chunk, "Attempted to remove a chunk from global interest that wasn't already in the interest");
                            unload.send(UnloadChunk::new(chunk));
                        }
                    }
                    for chunk in change.added.iter() {
                        if !set.insert(chunk) {
                            warn!(player = %change.player, chunk, "Attempted to add a chunk that was already in the interest");
                        }
                        chunks
                            .entry(chunk)
                            .and_modify(|(_, count)| {
                                // SAFETY: the count only increases here
                                *count = unsafe { NonZeroUsize::new_unchecked(count.get() + 1) };
                            })
                            .or_insert_with(|| {
                                load.send(ChunkRequest::new(chunk));
                                // SAFETY: 1 is not 0
                                (ChunkLoadingState::Queued, unsafe {
                                    NonZeroUsize::new_unchecked(1)
                                })
                            });
                    }
                } else {
                    for chunk in change.removed.iter() {
                        if let Some((_, count)) = chunks.get_mut(&chunk) {
                            if count.get() == 1 {
                                chunks.remove(&chunk);
                                unload.send(UnloadChunk::new(chunk));
                            } else {
                                // SAFETY: we already checked this
                                *count = unsafe { NonZeroUsize::new_unchecked(count.get() - 1) };
                            }
                        } else {
                            warn!(chunk, "Attempted to remove a chunk from global interest that wasn't already in the interest");
                            unload.send(UnloadChunk::new(chunk));
                        }
                    }
                    for chunk in change.added.iter() {
                        chunks
                            .entry(chunk)
                            .and_modify(|(_, count)| {
                                // SAFETY: the count only increases here
                                *count = unsafe { NonZeroUsize::new_unchecked(count.get() + 1) };
                            })
                            .or_insert_with(|| {
                                commands.trigger(ChunkRequest::new(chunk));
                                // SAFETY: 1 is not 0
                                (ChunkLoadingState::Queued, unsafe {
                                    NonZeroUsize::new_unchecked(1)
                                })
                            });
                    }
                }
            }
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
    start: AtomicUsize,
    end: AtomicUsize,
    body: [UnsafeCell<u64>; BUF_SIZE],
}
impl RingBufferBody {
    const fn new() -> Self {
        Self {
            cancel: AtomicBool::new(false),
            start: AtomicUsize::new(0),
            end: AtomicUsize::new(0),
            body: [const { UnsafeCell::new(0) }; BUF_SIZE],
        }
    }
    #[allow(clippy::mut_from_ref)]
    unsafe fn get(&self, idx: usize) -> &mut u64 {
        &mut *self.body[idx].get()
    }
    /// Pop an element from the buffer.
    fn pop(&self) -> Option<u64> {
        loop {
            let start = self.start.load(Ordering::Acquire);
            let end = self.end.load(Ordering::Acquire);
            if start == end {
                return None;
            }
            let new_start = (start + 1) % BUF_SIZE;
            if self
                .start
                .compare_exchange_weak(start, new_start, Ordering::AcqRel, Ordering::Acquire)
                .is_err()
            {
                continue;
            }
            return Some(unsafe { *self.get(start) });
        }
    }
    /// Fill with an iterator, this can cause logic errors if there are multiple writers.
    fn fill(&self, iter: &mut dyn Iterator<Item = u64>) -> usize {
        let start = self.start.load(Ordering::Acquire);
        let end = self.end.load(Ordering::Acquire);
        let rect_start = if start < end { start + BUF_SIZE } else { start };
        let mut count = 0;
        ((end + 1)..rect_start)
            .map(|i| i % BUF_SIZE)
            .zip(iter)
            .for_each(|(i, v)| {
                count += 1;
                unsafe { *self.get(i) = v };
            });
        self.end.store((end + count) % BUF_SIZE, Ordering::Release);
        count
    }
}
unsafe impl Sync for RingBufferBody {}

#[derive(Resource)]
pub struct ChunkloaderHandle {
    finished: Receiver<(u64, ChunkData)>,
    pool: CtxPool<Box<RingBufferBody>>,
    queue: PriorityQueue<u64, i32>,
}
impl ChunkloaderHandle {
    pub fn spawn(config: &WorldConfig, db: &Database) -> Self {
        let (finished_tx, finished_rx) = crossbeam_channel::bounded(8);
        let pool = CtxPool::new(
            Box::new(RingBufferBody::new()),
            std::thread::available_parallelism().map_or(1, |n| n.get()),
            || {
                let config = config.clone();
                let db = db.clone();
                let finished_tx = finished_tx.clone();
                move |queue, _| {
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
                }
            },
        );
        Self {
            pool,
            finished: finished_rx,
            queue: PriorityQueue::new(),
        }
    }
    pub fn finished(&self) -> impl Iterator<Item = (u64, ChunkData)> + '_ {
        self.finished.try_iter()
    }
    pub fn cancel(&self) {
        self.pool.ctx().cancel.store(true, Ordering::Release);
    }
}
impl Drop for ChunkloaderHandle {
    fn drop(&mut self) {
        self.cancel();
    }
}

pub fn load_chunks(
    mut commands: Commands,
    mut loaded_chunks: ResMut<LoadedChunks>,
    mut to_load: EventReader<ChunkRequest>,
    mut loader: ResMut<ChunkloaderHandle>,
) {
    for &ChunkRequest { id } in to_load.read() {
        if let Some((state, _)) = loaded_chunks.chunks.get_mut(&id) {
            match *state {
                ChunkLoadingState::Loaded(entity) => {
                    commands.trigger(ChunkLoaded { id, entity });
                    continue;
                }
                ChunkLoadingState::Loading => continue,
                ChunkLoadingState::Queued => *state = ChunkLoadingState::Loading,
            }
        }

        // TODO: priority queue
        loader.queue.push(id, 0);
        let loader = &mut *loader;
        loader
            .pool
            .ctx()
            .fill(&mut std::iter::from_fn(|| loader.queue.pop().map(|v| v.0)));
    }
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
