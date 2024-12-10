use bevy::log::*;
use bevy::tasks::futures_lite::Stream;
use futures_sink::Sink;
use std::pin::Pin;
use std::task::{Context, Poll};
use thiserror::Error;
use ws_stream_wasm::{WsErr, WsMessage, WsMeta, WsStream};

#[derive(Debug, Error)]
pub enum ConnectError {
    #[error(transparent)]
    Ws(#[from] WsErr),
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

#[derive(Debug)]
pub struct WebSocket {
    inner: WsStream,
    meta: WsMeta,
}
impl WebSocket {
    pub async fn connect(
        uri: &str,
        additional: impl IntoIterator<Item = &str>,
    ) -> Result<Self, ConnectError> {
        info!(%uri, "Connecting to server");
        WsMeta::connect(uri, additional.into_iter().collect::<Vec<_>>())
            .await
            .inspect_err(|err| error!(%err, "Error connecting to server"))
            .map(|(meta, inner)| Self { meta, inner })
            .map_err(ConnectError::Ws)
    }
    pub async fn close(&mut self) -> Result<(), WsErr> {
        self.meta.close().await.map(drop)
    }

    fn inner_pinned(self: Pin<&mut Self>) -> Pin<&mut WsStream> {
        unsafe { self.map_unchecked_mut(|s| &mut s.inner) }
    }
}
impl Stream for WebSocket {
    type Item = Vec<u8>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        match self.as_mut().inner_pinned().poll_next(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(None) => Poll::Ready(None),
            Poll::Ready(Some(WsMessage::Binary(v))) => Poll::Ready(Some(v)),
            Poll::Ready(Some(WsMessage::Text(v))) => Poll::Ready(Some(v.into())),
        }
    }
}
impl Sink<Vec<u8>> for WebSocket {
    type Error = WsErr;

    fn start_send(self: Pin<&mut Self>, item: Vec<u8>) -> Result<(), Self::Error> {
        self.inner_pinned().start_send(WsMessage::Binary(item))
    }
    fn poll_ready(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner_pinned().poll_ready(cx)
    }
    fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner_pinned().poll_close(cx)
    }
    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner_pinned().poll_flush(cx)
    }
}
