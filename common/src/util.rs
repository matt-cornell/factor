use bevy::log::*;
use bevy::tasks::futures_lite::Stream;
use futures_sink::Sink;
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::fmt::{self, Debug, Formatter};
use std::marker::PhantomData;
use std::ops::Deref;
use std::pin::Pin;
use std::task::{Context, Poll};
use triomphe::Arc;

/// Translate a stream/sink of bytes to messages.
#[derive(Debug, Clone)]
pub struct Translator<R, W, S> {
    inner: S,
    _marker: PhantomData<fn(W) -> R>,
}
impl<R, W, S> Translator<R, W, S> {
    /// Create a new translator wrapping a source.
    pub const fn new(inner: S) -> Self {
        Self {
            inner,
            _marker: PhantomData,
        }
    }
    /// Transmute this translator into a different one with different key/value types.
    pub fn transmute<R2, W2>(self) -> Translator<R2, W2, S> {
        Translator::new(self.inner)
    }

    /// Get the underlying `Stream`/`Sink`.
    pub fn inner(&self) -> &S {
        &self.inner
    }
    /// Get the underlying `Stream`/`Sink`.
    pub fn inner_mut(&mut self) -> &mut S {
        &mut self.inner
    }
    /// Get the underlying `Stream`/`Sink`, even if it doesn't implement `Unpin`.
    pub fn inner_pinned(self: Pin<&mut Self>) -> Pin<&mut S> {
        unsafe { self.map_unchecked_mut(|s| &mut s.inner) }
    }
}

impl<R: DeserializeOwned, W, S: Stream<Item: AsRef<[u8]>>> Stream for Translator<R, W, S> {
    type Item = R;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        match self.inner_pinned().poll_next(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(None) => Poll::Ready(None),
            Poll::Ready(Some(v)) => match postcard::from_bytes(v.as_ref()) {
                Ok(v) => Poll::Ready(Some(v)),
                Err(err) => {
                    warn!(%err, "Error deserializing packet");
                    Poll::Pending
                }
            },
        }
    }
}
impl<R, W: Serialize, S: Sink<Vec<u8>>> Sink<W> for Translator<R, W, S> {
    type Error = S::Error;

    fn start_send(self: Pin<&mut Self>, item: W) -> Result<(), Self::Error> {
        match postcard::to_stdvec(&item) {
            Ok(bytes) => self.inner_pinned().start_send(bytes),
            Err(err) => {
                error!(%err, "Error serializing object");
                Ok(())
            }
        }
    }
    fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner_pinned().poll_close(cx)
    }
    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner_pinned().poll_flush(cx)
    }
    fn poll_ready(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner_pinned().poll_ready(cx)
    }
}

pub struct InsideArc<T: ?Sized, B> {
    ptr: *const T,
    arc: Option<Arc<B>>,
}
impl<T: ?Sized, B> InsideArc<T, B> {
    pub fn new<F: FnOnce(&B) -> &T>(arc: Arc<B>, map: F) -> Self {
        Self {
            ptr: map(&arc) as *const T,
            arc: Some(arc),
        }
    }
    pub fn from_static(val: &'static T) -> Self {
        Self {
            ptr: val as *const T,
            arc: None,
        }
    }
    pub fn map<U: ?Sized, F: FnOnce(&T) -> &U>(this: Self, map: F) -> InsideArc<U, B> {
        InsideArc {
            ptr: map(&this) as *const U,
            arc: this.arc,
        }
    }
}
impl<T: ?Sized, B> Deref for InsideArc<T, B> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // SAFETY: we either borrow from `arc`, which we're keeping alive, or we have a static reference
        unsafe { &*self.ptr }
    }
}
impl<T: Debug + ?Sized, B> Debug for InsideArc<T, B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&**self, f)
    }
}
