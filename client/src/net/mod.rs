use crate::utils::ClientData;
use bevy::prelude::*;
use bevy::tasks::futures_lite::StreamExt;
use bevy::tasks::IoTaskPool;
use factor_common::glue::{
    ClientSide, ClientToServerMessage, PlayerData, ServerToClientMessage, StartingData,
};
use factor_common::rcu::Rcu;
use factor_common::util::Translator;
use futures_util::lock::Mutex;
use futures_util::SinkExt;
use std::borrow::Cow;
use std::io;
use std::sync::Arc as StdArc;
use triomphe::Arc;
use unsize::*;

#[cfg(target_family = "wasm")]
mod wasm;
#[cfg(target_family = "wasm")]
pub use wasm::*;

#[cfg(not(target_family = "wasm"))]
mod native;
#[cfg(not(target_family = "wasm"))]
pub use native::*;

#[derive(Debug)]
pub struct ClientConnection {
    inner: Mutex<Translator<ServerToClientMessage, ClientToServerMessage, WebSocket>>,
    player_data: Rcu<PlayerData>,
    starting_data: StartingData,
}
impl ClientConnection {
    pub async fn connect(
        uri: &str,
        additional: impl IntoIterator<Item = &str>,
    ) -> Result<Self, ConnectError> {
        let mut ws = Translator::new(WebSocket::connect(uri, additional).await?);
        let Some(ServerToClientMessage::Accept(starting_data, player_data)) = ws.next().await
        else {
            error!("First message from server wasn't an Accept");
            return Err(io::Error::new(io::ErrorKind::InvalidData, "").into());
        };
        Ok(Self {
            inner: Mutex::new(ws),
            starting_data,
            player_data: Rcu::new(Arc::new(player_data)),
        })
    }
}
#[async_trait::async_trait]
impl ClientSide for ClientConnection {
    async fn starting_info(&self) -> Cow<'_, StartingData> {
        Cow::Borrowed(&self.starting_data)
    }
    async fn set_vel(&self, vel: Vec3) {
        let res = self
            .inner
            .lock()
            .await
            .send(ClientToServerMessage::SetVel(vel))
            .await;
        if let Err(err) = res {
            error!(%err, "Error setting position");
        }
    }

    fn get_data(&self) -> Arc<dyn AsRef<PlayerData>> {
        self.player_data
            .read()
            .unsize(Coercion!(to dyn AsRef<PlayerData>))
    }

    fn startup(&self, _world: &mut World) {}
    fn shutdown(&self, world: &mut World) {
        world.commands().add(|world: &mut World| {
            let data = world
                .remove_resource::<ClientData>()
                .expect("ClientData resource should still exist!");
            let Ok(conn) = StdArc::downcast::<Self>(data.into_arc()) else {
                return;
            };
            IoTaskPool::get()
                .spawn(async move {
                    let mut conn = conn.inner.lock().await;
                    if let Err(err) = conn.send(ClientToServerMessage::Disconnect).await {
                        error!(%err, "Error closing connection");
                    }
                    let res = conn.inner_mut().close().await;
                    if let Err(err) = res {
                        error!(%err, "Error closing connection");
                    }
                })
                .detach();
        });
    }
}
