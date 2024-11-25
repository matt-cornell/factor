use async_trait::async_trait;
use bevy::prelude::*;
use std::any::Any;
use std::ops::Deref;
use std::sync::Arc;

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

#[derive(Clone, Resource)]
pub struct ClientData {
    inner: Arc<dyn ClientSide>,
}
impl ClientData {
    #[inline(always)]
    pub const fn new(inner: Arc<dyn ClientSide>) -> Self {
        Self { inner }
    }
    pub fn into_arc(self) -> Arc<dyn ClientSide> {
        self.inner
    }
}
impl Deref for ClientData {
    type Target = Arc<dyn ClientSide>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// Client side of the game. The client can only communicate through the server through this.
#[async_trait]
pub trait ClientSide: Any + Send + Sync + 'static {
    async fn server_version(&self) -> &str;
    fn startup(&self, world: &mut World);
    fn shutdown(&self, world: &mut World);
}

#[async_trait]
impl ClientSide for ServerDataInner {
    async fn server_version(&self) -> &str {
        env!("CARGO_PKG_VERSION")
    }
    fn startup(&self, _world: &mut World) {}
    fn shutdown(&self, _world: &mut World) {}
}

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClientToServerMessage {
    RequestVersion,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ServerToClientMessage {
    Version(String),
}
