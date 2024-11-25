use bevy::prelude::*;
use bevy::tasks::IoTaskPool;
use factor_common::glue::{ClientData, ClientSide, ClientToServerMessage, ServerToClientMessage};
use factor_common::util::Translator;
use futures_util::lock::Mutex;
use futures_util::SinkExt;
use std::fmt::Debug;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, OnceLock};
use std::task::{Context, Poll};

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
    version: OnceLock<String>,
}
impl ClientConnection {
    pub async fn connect(
        uri: &str,
        additional: impl IntoIterator<Item = &str>,
    ) -> Result<Self, ConnectError> {
        Ok(Self {
            inner: Mutex::new(Translator::new(WebSocket::connect(uri, additional).await?)),
            version: OnceLock::new(),
        })
    }
}
#[async_trait::async_trait]
impl ClientSide for ClientConnection {
    async fn server_version(&self) -> &str {
        if self.version.get().is_none() {
            let res = self
                .inner
                .lock()
                .await
                .send(ClientToServerMessage::RequestVersion)
                .await;
            if let Err(err) = res {
                error!(%err, "Failed to request version");
            }
        }
        GetServerVersion::new(self).await
    }
    fn startup(&self, _world: &mut World) {}
    fn shutdown(&self, world: &mut World) {
        world.commands().add(|world: &mut World| {
            let data = world
                .remove_resource::<ClientData>()
                .expect("ClientData resource should still exist!");
            let Ok(conn) = Arc::downcast::<Self>(data.into_arc()) else {
                return;
            };
            IoTaskPool::get()
                .spawn(async move {
                    let res = conn.inner.lock().await.inner_mut().close().await;
                    if let Err(err) = res {
                        error!(%err, "Error closing connection");
                    }
                })
                .detach();
        });
    }
}

struct GetServerVersion<'a> {
    conn: &'a ClientConnection,
}
impl<'a> GetServerVersion<'a> {
    pub fn new(conn: &'a ClientConnection) -> Self {
        Self { conn }
    }
}
impl<'a> Future for GetServerVersion<'a> {
    type Output = &'a str;
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if let Some(val) = self.conn.version.get() {
            Poll::Ready(val)
        } else {
            cx.waker().wake_by_ref();
            Poll::Pending
        }
    }
}
