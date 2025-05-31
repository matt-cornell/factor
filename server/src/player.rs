use crate::chunks::{chunk_height, ChunkCorners, ChunkInterest, ChunkLoaded, InterestChanged};
use crate::config::WorldConfig;
use crate::tables::PLAYERS;
use crate::utils::database::{self as redb, Database, TableResultExt};
use crate::utils::random_point_in_quadrilateral;
use bevy::prelude::*;
use factor_common::data::{PlayerId, Position};
use factor_common::mesh::MeshData;
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

#[derive(Debug, Clone, Copy, Component)]
pub struct PendingPosition(u64);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PlayerPosition {
    Resolved(Position),
    Pending(u64),
}
impl PlayerPosition {
    pub fn get_chunk(&self) -> u64 {
        match self {
            Self::Resolved(pos) => pos.get_chunk(),
            Self::Pending(chunk) => *chunk,
        }
    }
    pub fn is_pending(&self) -> bool {
        matches!(self, Self::Pending(_))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlayerData {
    pub pos: PlayerPosition,
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
            if let Some(table) = txn.open_table(PLAYERS).if_exists()? {
                if let Some(player) = table.get(id)? {
                    if let Some(data) = player.value() {
                        break 'load data;
                    }
                }
            }
            txn.close()?;
            let txn = db.begin_write()?;
            let mut table = txn.open_table(PLAYERS)?;
            let data = PlayerData {
                pos: PlayerPosition::Pending((chunk << 8) | rand::rng().random_range(0..256)),
            };
            let opt = Some(data);
            table.insert(id, &opt)?;
            drop(table);
            txn.commit()?;
            opt.unwrap()
        }
    };
    if let Err(err) = &res {
        error!(%id, %err, "Error loading player");
    }
    let res = match res {
        Ok(PlayerData { pos }) => {
            let mut cmd = commands.spawn((id, ChunkInterest::default()));
            let res = match pos {
                PlayerPosition::Resolved(pos) => {
                    cmd.insert(pos);
                    Some(Ok(cmd.id()))
                }
                PlayerPosition::Pending(chunk) => {
                    cmd.insert((PendingPosition(chunk), Position::default()));
                    None
                }
            };
            commands.send_event(InterestChanged {
                player: id,
                new: vec![pos.get_chunk()],
            });
            res
        }
        Err(err) => Some(Err(
            Arc::new(err).unsize(Coercion!(to dyn Error + Send + Sync))
        )),
    };
    if let Some(res) = res {
        commands.trigger(PlayerLoaded { id, res });
    }
}

pub fn set_heights(
    trig: Trigger<ChunkLoaded>,
    mut commands: Commands,
    mut players: Query<(Entity, Option<&PlayerId>, &PendingPosition, &mut Position)>,
    chunks: Query<(&MeshData, &ChunkCorners)>,
) {
    let Ok((mesh, &ChunkCorners { corners })) = chunks
        .get(trig.entity)
        .inspect_err(|err| error!(%err, "Tried to set heights for a chunk that isn't loaded"))
    else {
        return;
    };
    for (entity, id, &PendingPosition(chunk), mut pos) in players.iter_mut() {
        if chunk != trig.id {
            continue;
        }
        let mut rng = rand::rng();
        let mut xz;
        let y = loop {
            xz = random_point_in_quadrilateral(corners, &mut rng);
            if let Some(y) = chunk_height(xz, mesh) {
                break y;
            }
        };
        *pos = Position {
            frame: chunk >> 8,
            pos: xz.extend(y).xzy(),
            rot: Quat::from_rotation_y(rng.random_range(0.0..=TAU)) * Quat::from_rotation_x(0.2),
        };
        commands.entity(entity).remove::<PendingPosition>();
        if let Some(&id) = id {
            info!(%id, pos = %pos.pos, "Setting player position");
            commands.trigger(PlayerLoaded {
                id,
                res: Ok(entity),
            });
        }
    }
}

pub fn persist_players(
    players: Query<(&PlayerId, &Position), (Without<PendingPosition>, Changed<Position>)>,
    db: Res<Database>,
) {
    for (&id, &pos) in players.iter() {
        let res: Result<(), redb::Error> = try {
            let txn = db.begin_write()?;
            let mut table = txn.open_table(PLAYERS)?;
            table.insert(
                id,
                Some(PlayerData {
                    pos: PlayerPosition::Resolved(pos),
                }),
            )?;
            drop(table);
            txn.commit()?;
        };
        if let Err(err) = res {
            error!(%err, ?id, "Error saving player data");
        }
    }
}
