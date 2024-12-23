use crate::config::WorldConfig;
use crate::tables::{NoiseLocation, CHUNKS, HIGH_GRAD_NOISE, HIGH_VALUE_NOISE, TERRAIN};
use crate::terrain::climate::ClimateCell;
use crate::utils::database::Database;
use crate::utils::mesh::*;
use bevy::prelude::*;
use factor_common::cell::{corners_of, ChunkId};
use factor_common::coords::{get_absolute, get_relative, LonLat};
use factor_common::healpix;
use rand::prelude::*;
use redb::{ReadableTable, Table};
use serde::{Deserialize, Serialize};
use std::io;
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

#[derive(Debug, Clone, Copy, PartialEq, Event)]
pub struct ChunkLoaded {
    pub id: u64,
    pub entity: Entity,
}

pub fn load_chunk(
    trig: Trigger<ChunkRequest>,
    mut commands: Commands,
    cfg: Res<WorldConfig>,
    db: Res<Database>,
) {
    let id = trig.id;
    let res: Result<(), redb::Error> = try {
        let txn = db.begin_read()?;
        match txn.open_table(CHUNKS) {
            Ok(table) => {
                if let Some(chunk) = table.get(id)? {
                    if let Some(data) = chunk.value() {
                        let entity = commands.spawn((ChunkId(id), data.surface)).id();
                        commands.trigger(ChunkLoaded { id, entity });
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
            &cfg,
            id,
            &mut txn.open_table(TERRAIN)?,
            &mut txn.open_table(HIGH_VALUE_NOISE)?,
            &mut txn.open_table(HIGH_GRAD_NOISE)?,
        )?;
        let opt_data = Some(ChunkData { surface });
        table.insert(id, &opt_data)?;
        let entity = commands
            .spawn((ChunkId(id), opt_data.unwrap().surface))
            .id();
        commands.trigger(ChunkLoaded { id, entity });
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
        let layer = healpix::Layer::new(12);
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
                    debug!(
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
    let center = healpix::Layer::new(12).center(hash);
    let surface = try_mesh_quad(chunk_corners, 32, |MeshPoint { abs, .. }| {
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
