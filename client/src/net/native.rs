use async_net::TcpStream;
use async_tungstenite::async_tls::{client_async_tls, ClientStream};
use async_tungstenite::tungstenite::client::IntoClientRequest;
use async_tungstenite::tungstenite::error::UrlError;
use async_tungstenite::tungstenite::http::{HeaderValue, Uri};
use async_tungstenite::tungstenite::{self, Message, Result};
use async_tungstenite::WebSocketStream;
use bevy::log::*;
use bevy::tasks::futures_lite::Stream;
use futures_sink::Sink;
use std::fmt::Debug;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

type Inner = WebSocketStream<ClientStream<TcpStream>>;
pub type ConnectError = tungstenite::Error;
pub use tungstenite::Bytes;

#[derive(Debug)]
pub struct WebSocket {
    inner: Inner,
}
impl WebSocket {
    pub async fn connect(uri: &str, additional: impl IntoIterator<Item = &str>) -> Result<Self> {
        info!(%uri, "Connecting to server");

        let uri = uri
            .parse::<Uri>()
            .inspect_err(|err| error!(%err, "Error parsing URI"))?;

        let Some(addr) = uri.host() else {
            let err = tungstenite::Error::Url(UrlError::NoHostName);
            error!(%err, "Error parsing URI");
            return Err(err);
        };
        let port = uri.port_u16();
        let res = if let Some(port) = port {
            TcpStream::connect((addr, port)).await
        } else {
            TcpStream::connect(addr).await
        };
        let stream = res.inspect_err(|err| error!(?addr, %err, "Error connecting to server"))?;
        let mut req = uri.into_client_request()?;
        for data in additional {
            req.headers_mut().append(
                "Sec-WebSocket-Protocol",
                HeaderValue::from_str(data)
                    .inspect_err(|err| error!(%err, "Invalid header value for request"))?,
            );
        }
        let (inner, _) = client_async_tls(req, stream)
            .await
            .inspect_err(|err| error!(%err, "Error connecting to server"))?;
        Ok(Self { inner })
    }

    pub async fn close(&mut self) -> Result<()> {
        self.inner.close(None).await
    }

    fn inner_pinned(self: Pin<&mut Self>) -> Pin<&mut Inner> {
        unsafe { self.map_unchecked_mut(|s| &mut s.inner) }
    }
}
impl Stream for WebSocket {
    type Item = Bytes;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        match self.as_mut().inner_pinned().poll_next(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(None) => Poll::Ready(None),
            Poll::Ready(Some(Ok(Message::Binary(v)))) => Poll::Ready(Some(v)),
            Poll::Ready(Some(Ok(Message::Text(v)))) => Poll::Ready(Some(v.into())),
            Poll::Ready(Some(Ok(Message::Frame(_)))) => {
                unreachable!("This will never be read when reading a message")
            }
            Poll::Ready(Some(Ok(Message::Ping(v)))) => {
                trace!(?v, "Received ping"); // TODO: send pong
                Poll::Pending
            }
            Poll::Ready(Some(Ok(Message::Pong(v)))) => {
                trace!(?v, "Received pong");
                Poll::Pending
            }
            Poll::Ready(Some(Ok(Message::Close(_)))) => {
                info!("Closing websocket");
                Poll::Ready(None)
            }
            Poll::Ready(Some(Err(err))) => {
                error!(%err, "Error reading from websocket");
                Poll::Pending
            }
        }
    }
}
impl Sink<Vec<u8>> for WebSocket {
    type Error = tungstenite::Error;

    fn start_send(self: Pin<&mut Self>, item: Vec<u8>) -> Result<(), Self::Error> {
        self.inner_pinned()
            .start_send(Message::Binary(Bytes::from_owner(item)))
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
impl Sink<Bytes> for WebSocket {
    type Error = tungstenite::Error;

    fn start_send(self: Pin<&mut Self>, item: Bytes) -> Result<(), Self::Error> {
        self.inner_pinned().start_send(Message::Binary(item))
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
