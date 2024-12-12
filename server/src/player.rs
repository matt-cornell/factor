use crate::tables::PLAYERS;
use crate::terrain::climate::ClimateFlags;
use crate::utils::database::{self as redb, Database};
use crate::utils::random_point_in_quadrilateral;
use crate::ClimateData;
use bevy::prelude::*;
use factor_common::PlayerId;
use itertools::Itertools;
use rand::prelude::*;
use serde::{Deserialize, Serialize};
use std::error::Error;
use triomphe::Arc;
use unsize::*;

#[derive(Debug, Default, Clone, Copy)]
pub struct PlayerIdKey;
impl redb::Value for PlayerIdKey {
    type AsBytes<'a> = [u8; 32];
    type SelfType<'a> = PlayerId;

    fn as_bytes<'a, 'b: 'a>(value: &'a Self::SelfType<'b>) -> Self::AsBytes<'a>
    where
        Self: 'a,
        Self: 'b,
    {
        value.as_bytes()
    }
    fn from_bytes<'a>(data: &'a [u8]) -> Self::SelfType<'a>
    where
        Self: 'a,
    {
        PlayerId::from_raw(data[..32].try_into().unwrap())
    }
    fn fixed_width() -> Option<usize> {
        Some(32)
    }
    fn type_name() -> redb::TypeName {
        redb::TypeName::new("factor::PlayerId")
    }
}
impl redb::Key for PlayerIdKey {
    fn compare(data1: &[u8], data2: &[u8]) -> std::cmp::Ordering {
        data1[..32].cmp(&data2[..32])
    }
}

/// Data about the player that gets serialized and deserialzied.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlayerData {
    pub chunk: u64,
    pub pos: Vec3,
}

/// State for a player, with their data and actual
#[derive(Debug, Clone, Component)]
pub struct PlayerState {
    pub id: PlayerId,
    pub interest: tinyset::SetU64,
    pub data: PlayerData,
}

/// A request for a player to be loaded.
#[derive(Debug, Clone, Copy, PartialEq, Event)]
pub struct PlayerRequest(pub PlayerId);

/// A player has been loaded.
#[derive(Debug, Clone, Event)]
pub struct PlayerLoaded {
    pub id: PlayerId,
    pub res: Result<(), Arc<dyn Error + Send + Sync>>,
}

pub fn load_player(
    trigger: Trigger<PlayerRequest>,
    db: Res<Database>,
    world: Res<ClimateData>,
    mut commands: Commands,
) {
    let id = trigger.event().0;
    let res: Result<_, redb::Error> = try {
        'load: {
            let txn = db.begin_read()?;
            match txn.open_table(PLAYERS) {
                Ok(table) => {
                    if let Some(player) = table.get(id)? {
                        if let Some(data) = player.value() {
                            break 'load data;
                        }
                    }
                }
                Err(redb::TableError::TableDoesNotExist(_)) => {}
                Err(err) => Err(err)?,
            }
            txn.close()?;
            let txn = db.begin_write()?;
            let mut table = txn.open_table(PLAYERS)?;
            let mut rng = thread_rng();
            let clim_cell = *world
                .cells
                .iter()
                .positions(|c| !c.flags.contains(ClimateFlags::OCEAN)) // TODO: better starting conditions
                .map(|i| i as u64)
                .collect::<Vec<_>>()
                .choose(&mut rng)
                .unwrap();
            let chunk = if let Some(depth_diff) = 16u8.checked_sub(world.depth) {
                let additional = rng.gen_range(0..(1 << depth_diff));
                clim_cell << (depth_diff * 2) | additional
            } else {
                clim_cell >> ((world.depth - 16) * 2)
            };
            let coords =
                random_point_in_quadrilateral(factor_common::cell::corners_of(16, chunk), &mut rng);
            let data = PlayerData {
                chunk,
                pos: coords.extend(0.0).xzy(),
            };
            let opt = Some(data);
            table.insert(id, &opt)?;
            #[allow(clippy::unnecessary_literal_unwrap)]
            opt.unwrap()
        }
    };
    if let Err(err) = &res {
        error!(%id, %err, "Error loading player");
    }
    let res = match res {
        Ok(data) => {
            commands.spawn(PlayerState {
                id,
                data,
                interest: tinyset::SetU64::new(),
            });
            Ok(())
        }
        Err(err) => Err(Arc::new(err).unsize(Coercion!(to dyn Error + Send + Sync))),
    };
    commands.trigger(PlayerLoaded { id, res });
}