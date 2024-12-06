use crate::player::{finish_player, PlayerDataExt};
use crate::tables::PLAYERS;
use crate::utils::database::*;
use crate::ServerSystems;
use async_trait::async_trait;
use bevy::prelude::*;
use factor_common::glue::*;
use factor_common::rcu::{OptionRcu, Rcu};
use futures_util::lock::Mutex;
use papaya::HashMap;
use std::borrow::Cow;
use std::ops::Deref;
use triomphe::Arc;
use unsize::*;

/// Inner data for the server.
#[derive(Debug)]
pub struct ServerDataInner {
    pub is_ok: bool,
    pub starting_info: StartingData,
    pub other_players: HashMap<String, Rcu<PlayerDataExt>>,
}
impl FromWorld for ServerData {
    fn from_world(world: &mut World) -> Self {
        let db = world.resource::<Database>();
        let mut is_ok = true;
        let mut default_player = OptionRcu::none();
        let other_players = HashMap::new();
        let starting_info = StartingData {
            version: env!("CARGO_PKG_VERSION").to_string(),
            chunk_depth: 16,
        };
        match db.begin_read() {
            Ok(txn) => {
                match txn.open_table(PLAYERS) {
                    Ok(table) => match table.iter() {
                        Ok(iter) => {
                            (&other_players).extend(iter.filter_map(|res| match res {
                                Ok((k, v)) => {
                                    let key = k.value();
                                    let val = v.value()?.into();
                                    if key.is_empty() {
                                        default_player.write_mut(Some(Arc::new(val)));
                                        None
                                    } else {
                                        Some((key, Rcu::new(Arc::new(val))))
                                    }
                                }
                                Err(err) => {
                                    error!(%err, "Error iterating over table");
                                    None
                                }
                            }));
                        }
                        Err(err) => {
                            error!(%err, "Error iterating over table");
                        }
                    },
                    Err(redb::TableError::TableDoesNotExist(_)) => {
                        warn!("player table does not exist, skipping");
                    }
                    Err(err) => {
                        error!(%err, "Error opening player table");
                    }
                }
                if let Err(err) = txn.close() {
                    error!(%err, "Error finishing read from database");
                    is_ok = false;
                }
            }
            Err(err) => {
                error!(%err, "Error reading from database");
                is_ok = false;
            }
        }
        ServerData {
            inner: Arc::new(LockedServerData {
                locked: Mutex::new(ServerDataInner {
                    is_ok,
                    starting_info,
                    other_players,
                }),
                player: default_player,
            }),
        }
    }
}

/// A handle to the server data, wrapped in an `Arc` for easy cloning.
#[derive(Debug, Clone, Resource)]
pub struct ServerData {
    inner: Arc<LockedServerData>,
}
impl ServerData {
    /// Convert this into a client-side handle.
    pub fn client_side(&self) -> Arc<dyn ClientSide> {
        Arc::clone(&self.inner).unsize(Coercion!(to dyn ClientSide))
    }
}
impl Deref for ServerData {
    type Target = LockedServerData;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// This is a wrapper around `Mutex<ServerDataInner>` that allows me to implement `ClientSide`
#[derive(Debug)]
pub struct LockedServerData {
    locked: Mutex<ServerDataInner>,
    player: OptionRcu<PlayerDataExt>,
}
impl Deref for LockedServerData {
    type Target = Mutex<ServerDataInner>;

    fn deref(&self) -> &Self::Target {
        &self.locked
    }
}

#[async_trait]
impl ClientSide for LockedServerData {
    async fn starting_info(&self) -> Cow<'_, StartingData> {
        Cow::Owned(self.lock().await.starting_info.clone())
    }
    async fn set_vel(&self, vel: Vec3) {
        self.player.update(|player| {
            player.as_mut().unwrap().shared.vel = vel;
        });
    }

    fn get_data(&self) -> Arc<dyn AsRef<PlayerData>> {
        self.player
            .read()
            .unwrap()
            .unsize(Coercion!(to dyn AsRef<PlayerData>))
    }

    fn startup(&self, world: &mut World) {
        let &ServerSystems { new_player } = world.resource::<ServerSystems>();
        self.player.update(|player| {
            if let Some(player) = player {
                finish_player(world, player)
            } else {
                *player = Some(world.run_system(new_player).unwrap());
            }
        });
    }
    fn shutdown(&self, _world: &mut World) {}
}
