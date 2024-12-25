use crate::config::WorldConfig;
use crate::tables::{NoiseLocation, CHUNKS, HIGH_GRAD_NOISE, HIGH_VALUE_NOISE, TERRAIN};
use crate::terrain::climate::ClimateCell;
use crate::utils::database::Database;
use crate::utils::mesh::*;
use bevy::prelude::*;
use bevy::utils::tracing::Span;
use bevy::utils::HashMap;
use factor_common::cell::corners_of;
use factor_common::coords::{get_absolute, get_relative, LonLat};
use factor_common::data::{ChunkId, ChunkInterest, PlayerId};
use factor_common::healpix;
use rand::prelude::*;
use rayon::prelude::*;
use redb::{ReadableTable, Table};
use serde::{Deserialize, Serialize};
use std::io;
use std::num::NonZeroUsize;
use std::ops::{Add, Mul};

#[derive(Debug, Serialize, Deserialize)]
pub struct ChunkData {
    pub surface: MeshData,
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

/// A map of chunks to how many players want them loaded.
#[derive(Debug, Default, Clone, Resource)]
pub struct LoadedChunks {
    pub chunks: HashMap<u64, NonZeroUsize>,
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
                    if let Some(count) = chunks.get_mut(&chunk) {
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
                        .and_modify(|count| {
                            // SAFETY: the count only increases here
                            *count = unsafe { NonZeroUsize::new_unchecked(count.get() + 1) };
                        })
                        .or_insert_with(|| {
                            load.send(ChunkRequest::new(chunk));
                            // SAFETY: 1 is not 0
                            unsafe { NonZeroUsize::new_unchecked(1) }
                        });
                }
            } else {
                for chunk in change.removed.iter() {
                    if let Some(count) = chunks.get_mut(&chunk) {
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
                        .and_modify(|count| {
                            // SAFETY: the count only increases here
                            *count = unsafe { NonZeroUsize::new_unchecked(count.get() + 1) };
                        })
                        .or_insert_with(|| {
                            load.send(ChunkRequest::new(chunk));
                            // SAFETY: 1 is not 0
                            unsafe { NonZeroUsize::new_unchecked(1) }
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
                        if let Some(count) = chunks.get_mut(&chunk) {
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
                            .and_modify(|count| {
                                // SAFETY: the count only increases here
                                *count = unsafe { NonZeroUsize::new_unchecked(count.get() + 1) };
                            })
                            .or_insert_with(|| {
                                load.send(ChunkRequest::new(chunk));
                                // SAFETY: 1 is not 0
                                unsafe { NonZeroUsize::new_unchecked(1) }
                            });
                    }
                } else {
                    for chunk in change.removed.iter() {
                        if let Some(count) = chunks.get_mut(&chunk) {
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
                            .and_modify(|count| {
                                // SAFETY: the count only increases here
                                *count = unsafe { NonZeroUsize::new_unchecked(count.get() + 1) };
                            })
                            .or_insert_with(|| {
                                commands.trigger(ChunkRequest::new(chunk));
                                // SAFETY: 1 is not 0
                                unsafe { NonZeroUsize::new_unchecked(1) }
                            });
                    }
                }
            }
        }
    }
}

pub fn unload_chunks(
    mut commands: Commands,
    query: Query<(Entity, &ChunkId)>,
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

pub fn load_chunks(
    commands: ParallelCommands,
    cfg: Res<WorldConfig>,
    db: Res<Database>,
    mut to_load: EventReader<ChunkRequest>,
    mut queue: Local<
        Option<(
            crossbeam_channel::Sender<u64>,
            crossbeam_channel::Receiver<u64>,
        )>,
    >,
) {
    use std::time::Duration;
    use wasm_timer::Instant;
    const TIMEOUT: Duration = Duration::from_millis(100);
    let span = Span::current();
    let (tx, rx) = queue.get_or_insert_with(crossbeam_channel::unbounded);
    let start = Instant::now();
    let has_time = rx
        .try_iter()
        .par_bridge()
        .try_for_each(|id| {
            let _guard = span.enter();
            let step = Instant::now();
            load_chunk(id, &commands, &cfg, &db);
            debug!(id, time = ?step.elapsed(), "Loaded chunk");
            (start.elapsed() < TIMEOUT).then_some(())
        })
        .is_some();
    if has_time {
        to_load.par_read().for_each(|&ChunkRequest { id }| {
            if start.elapsed() < TIMEOUT {
                let _guard = span.enter();
                let step = Instant::now();
                load_chunk(id, &commands, &cfg, &db);
                debug!(id, time = ?step.elapsed(), "Loaded chunk");
            } else if let Err(err) = tx.send(id) {
                error!(%err, "Error queuing chunk load request");
            }
        });
    } else {
        warn!(
            queued = rx.len(),
            new = to_load.len(),
            "Chunk loading behind"
        );
        to_load.par_read().for_each(|req| {
            if let Err(err) = tx.send(req.id) {
                error!(%err, "Error queuing chunk load request");
            }
        });
    }
}

pub fn load_chunk(id: u64, commands: &ParallelCommands, cfg: &WorldConfig, db: &Database) {
    let res: Result<(), redb::Error> = try {
        let txn = db.begin_read()?;
        match txn.open_table(CHUNKS) {
            Ok(table) => {
                if let Some(chunk) = table.get(id)? {
                    if let Some(data) = chunk.value() {
                        commands.command_scope(|mut commands| {
                            let entity = commands.spawn((ChunkId(id), data.surface)).id();
                            commands.trigger(ChunkLoaded { id, entity });
                        });
                        drop(table);
                        txn.close()?;
                        return;
                    }
                }
            }
            Err(redb::TableError::TableDoesNotExist(_)) => {}
            Err(err) => Err(err)?,
        }
        txn.close()?;
        let txn = db.begin_write()?;
        let mut table = txn.open_table(CHUNKS)?;
        let surface = setup_chunk(
            cfg,
            id,
            &mut txn.open_table(TERRAIN)?,
            &mut txn.open_table(HIGH_VALUE_NOISE)?,
            &mut txn.open_table(HIGH_GRAD_NOISE)?,
        )?;
        let opt_data = Some(ChunkData { surface });
        table.insert(id, &opt_data)?;
        drop(table);
        txn.commit()?;
        commands.command_scope(|mut commands| {
            let entity = commands
                .spawn((ChunkId(id), opt_data.unwrap().surface))
                .id();
            commands.trigger(ChunkLoaded { id, entity });
        });
    };
    if let Err(err) = res {
        error!(id, %err, "Error loading chunk");
    }
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

fn get_height(
    config: &WorldConfig,
    coords: LonLat,
    terrain: &mut Table<u64, ClimateCell>,
    high_value: &mut Table<NoiseLocation, f32>,
    high_grad: &mut Table<NoiseLocation, [f32; 2]>,
) -> Result<f32, redb::Error> {
    let mut height = {
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
        sum / scale
    };
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
                let opt = high_grad.get(loc)?;
                let val = if let Some(v) = opt {
                    v.value()
                } else {
                    drop(opt);
                    trace!(
                        layer = n,
                        cell,
                        gradient = true,
                        "Generating new random weight"
                    );
                    let mut rng = get_rng(config.seed.unwrap(), loc.layer, loc.cell);
                    let v = rng.sample(rand_distr::UnitCircle);
                    high_grad.insert(loc, v)?;
                    v
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
                let opt = high_value.get(loc)?;
                let val = if let Some(v) = opt {
                    v.value()
                } else {
                    drop(opt);
                    debug!(
                        layer = n,
                        cell,
                        gradient = false,
                        "Generating new random weight"
                    );
                    let mut rng = get_rng(config.seed.unwrap(), loc.layer, loc.cell);
                    let v = rng.gen();
                    high_value.insert(loc, v)?;
                    v
                };
                sum += w;
                scale += val * w;
            }
        }
        height += sum / scale;
    }
    Ok(height)
}

fn setup_chunk(
    config: &WorldConfig,
    hash: u64,
    terrain: &mut Table<u64, ClimateCell>,
    high_value: &mut Table<NoiseLocation, f32>,
    high_grad: &mut Table<NoiseLocation, [f32; 2]>,
) -> Result<MeshData, redb::Error> {
    use factor_common::healpix::nested::zordercurve::*;
    let zoc = get_zoc(4);
    let base_hash = hash >> 8;
    let base_corners = corners_of(12, base_hash); // SENW
    let ij = zoc.h2ij(hash & 0xff);
    let i = zoc.ij2i(ij);
    let j = zoc.ij2j(ij);
    let chunk_corners = std::array::from_fn(|n| {
        let (i, j) = match n {
            0 => (i, j),
            1 => (i + 1, j),
            2 => (i + 1, j + 1),
            3 => (i, j + 1),
            _ => unreachable!(),
        };
        let i = i as f32 * 0.5;
        let j = j as f32 * 0.5;
        bilinear(i, j, base_corners)
    });
    let center = healpix::Layer::new(12).center(base_hash);
    let surface = try_mesh_quad(chunk_corners, 64, |MeshPoint { abs, .. }| {
        get_height(
            config,
            get_absolute(center, abs.into()),
            terrain,
            high_value,
            high_grad,
        )
    })?;
    Ok(surface)
}

fn bilinear<T: Mul<f32, Output = T> + Add<Output = T>>(i: f32, j: f32, verts: [T; 4]) -> T {
    let [s, e, n, w] = verts;
    let se = e * i + s * (1.0 - i);
    let nw = n * i + w * (1.0 - i);
    nw * j + se * (1.0 - j)
}
