use async_trait::async_trait;
use bevy::prelude::*;
use serde::{Deserialize, Serialize};
use std::any::Any;
use std::borrow::Cow;
use triomphe::Arc;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StartingData {
    pub version: String,
    pub chunk_depth: u8,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlayerData {
    pub chunk: u64,
    pub pos: Vec3,
    pub vel: Vec3,
}
impl AsRef<PlayerData> for PlayerData {
    fn as_ref(&self) -> &PlayerData {
        self
    }
}

/// Client side of the game. The client can only communicate through the server through this.
#[async_trait]
pub trait ClientSide: Any + Send + Sync + 'static {
    async fn starting_info(&self) -> Cow<'_, StartingData>;
    async fn set_vel(&self, vel: Vec3);

    fn get_data(&self) -> Arc<dyn AsRef<PlayerData>>;

    fn startup(&self, world: &mut World);
    fn shutdown(&self, world: &mut World);
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClientToServerMessage {
    Disconnect,
    SetVel(Vec3),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ServerToClientMessage {
    Accept(StartingData, PlayerData),
    SetPos(u64, Vec3),
    SetVel(Vec3),
}
