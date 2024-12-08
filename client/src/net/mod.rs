use bevy::prelude::*;
use bevy::tasks::futures_lite::{future, StreamExt};
use factor_common::glue::{ClientToServerMessage, ServerToClientMessage};
use factor_common::util::Translator;
use futures_util::lock::{Mutex, MutexGuard};

#[cfg(target_family = "wasm")]
mod wasm;
use futures_util::SinkExt;
#[cfg(target_family = "wasm")]
pub use wasm::*;

#[cfg(not(target_family = "wasm"))]
mod native;
#[cfg(not(target_family = "wasm"))]
pub use native::*;

pub type TranslatedStream = Translator<ServerToClientMessage, ClientToServerMessage, WebSocket>;

#[derive(Debug, Resource)]
pub struct ClientConnection {
    inner: Mutex<TranslatedStream>,
}
impl ClientConnection {
    pub async fn connect(
        uri: &str,
        additional: impl IntoIterator<Item = &str>,
    ) -> Result<Self, ConnectError> {
        let ws = Translator::new(WebSocket::connect(uri, additional).await?);
        Ok(Self {
            inner: Mutex::new(ws),
        })
    }
    pub async fn lock(&self) -> MutexGuard<TranslatedStream> {
        self.inner.lock().await
    }
    pub async fn read(&self) -> Option<ServerToClientMessage> {
        self.inner.lock().await.next().await
    }
    pub async fn write(&self, msg: ClientToServerMessage) {
        if let Err(err) = self.inner.lock().await.send(msg).await {
            error!(%err, "Error sending message");
        }
    }
    /// Try to read a message synchronously. Returns `None` if none are available, or `Some(None)` if the connection is closed
    pub fn try_read(&self) -> Option<Option<ServerToClientMessage>> {
        let mut guard = self.inner.try_lock()?;
        future::block_on(future::poll_once(guard.next()))
    }
}
