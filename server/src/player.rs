use crate::chunk::ChunkLoaded;
use crate::config::WorldConfig;
use crate::tables::PLAYERS;
use crate::utils::database::{self as redb, Database};
use crate::utils::random_point_in_quadrilateral;
use bevy::prelude::*;
use factor_common::data::{ChunkInterest, PlayerId, Position};
use rand::prelude::*;
use serde::{Deserialize, Serialize};
use std::error::Error;
use std::f32::consts::TAU;
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlayerData {
    pos: Position,
}

#[derive(Debug, Default, Clone, Bundle)]
pub struct PlayerBundle {
    pub id: PlayerId,
    pub pos: Position,
    pub interest: ChunkInterest,
}

/// A request for a player to be loaded.
#[derive(Debug, Clone, Copy, PartialEq, Event)]
pub struct PlayerRequest(pub PlayerId);

/// A player has been loaded.
#[derive(Debug, Clone, Event)]
pub struct PlayerLoaded {
    pub id: PlayerId,
    pub res: Result<Entity, Arc<dyn Error + Send + Sync>>,
}

/// A marker component for players that need their height to be calculated
#[derive(Debug, Default, Clone, Copy, Component)]
pub struct NeedsHeight;

pub fn load_player(
    trigger: Trigger<PlayerRequest>,
    cfg: Res<WorldConfig>,
    db: Res<Database>,
    mut commands: Commands,
) {
    let Some(chunk) = cfg.spawn else {
        error!("Spawn chunk isn't set!");
        return;
    };
    let id = trigger.event().0;
    let res: Result<_, redb::Error> = try {
        'load: {
            let txn = db.begin_read()?;
            match txn.open_table(PLAYERS) {
                Ok(table) => {
                    if let Some(player) = table.get(id)? {
                        if let Some(data) = player.value() {
                            break 'load (data, false);
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
            let coords =
                random_point_in_quadrilateral(factor_common::cell::corners_of(12, chunk), &mut rng);
            let pos = Position {
                frame: chunk,
                pos: coords.extend(0.0).xzy(),
                rot: Quat::from_rotation_y(rng.gen_range(0.0..=TAU)) * Quat::from_rotation_x(0.2),
            };
            let data = PlayerData { pos };
            let opt = Some(data);
            table.insert(id, &opt)?;
            drop(table);
            txn.commit()?;
            (opt.unwrap(), true)
        }
    };
    if let Err(err) = &res {
        error!(%id, %err, "Error loading player");
    }
    let res = match res {
        Ok((data, needs_height)) => {
            let mut cmd = commands.spawn(PlayerBundle {
                id,
                pos: data.pos,
                ..default()
            });
            if needs_height {
                cmd.insert(NeedsHeight);
            }
            Ok(cmd.id())
        }
        Err(err) => Err(Arc::new(err).unsize(Coercion!(to dyn Error + Send + Sync))),
    };
    commands.trigger(PlayerLoaded { id, res });
}

pub fn set_heights(
    trig: Trigger<ChunkLoaded>,
    mut commands: Commands,
    mut players: Query<(Entity, &mut Position), With<NeedsHeight>>,
) {
    let frame = trig.id >> 8;
    for (entitiy, mut player) in players.iter_mut() {
        if player.frame == frame && player.get_chunk() == trig.id {
            player.pos.y = 10.0; // TODO: set an actual height
            commands.entity(entitiy).remove::<NeedsHeight>();
        }
    }
}
