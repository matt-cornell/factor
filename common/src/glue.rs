use bevy::math::Vec3;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClientToServerMessage {
    Disconnect,
    SetVel(Vec3),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ServerToClientMessage {
    Accept,
    SetPos(u64, Vec3),
    SetVel(Vec3),
}
