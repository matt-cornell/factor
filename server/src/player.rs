use bevy::prelude::*;
use factor_common::glue::PlayerData;
use serde::{Deserialize, Serialize};

#[inline(always)]
const fn default_entity() -> Entity {
    Entity::PLACEHOLDER
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlayerDataExt {
    #[serde(flatten)]
    pub shared: PlayerData,
    #[serde(skip, default = "default_entity")]
    pub id: Entity,
}
impl From<PlayerData> for PlayerDataExt {
    fn from(value: PlayerData) -> Self {
        Self {
            shared: value,
            id: Entity::PLACEHOLDER,
        }
    }
}
impl AsRef<PlayerData> for PlayerDataExt {
    fn as_ref(&self) -> &PlayerData {
        &self.shared
    }
}

pub fn new_player() -> PlayerDataExt {
    todo!()
}
pub fn finish_player(world: &mut World, player: &mut PlayerDataExt) {
    player.id = world.spawn_empty().id();
}
