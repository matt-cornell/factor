use std::ops::Deref;
use std::sync::Arc;

use async_trait::async_trait;
use bevy::prelude::*;

/// Inner data for the server.
#[derive(Debug, Default, Clone)]
pub struct ServerDataInner {}

/// A handle to the server data, wrapped in an `Arc` for easy cloning.
#[derive(Debug, Default, Clone, Resource)]
pub struct ServerData {
    inner: Arc<ServerDataInner>,
}
impl ServerData {
    /// Convert this into a client-side handle.
    pub fn client_side(&self) -> Arc<dyn ClientSide> {
        self.inner.clone()
    }
}
impl Deref for ServerData {
    type Target = ServerDataInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// Client side of the game. The client can only communicate through the server through this.
#[async_trait]
pub trait ClientSide: Send + Sync + 'static {
    async fn server_version(&self) -> &str;
    async fn update(&self, world: &mut World);
}

#[async_trait]
impl ClientSide for ServerDataInner {
    async fn server_version(&self) -> &str {
        env!("CARGO_PKG_VERSION")
    }
    async fn update(&self, _world: &mut World) {}
}

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClientToServerMessage {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ServerToClientMessage {}
