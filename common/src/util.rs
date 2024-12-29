use bevy::ecs::world::{Command, World};
use bevy::log::*;
use bevy::state::state::StateTransition;
use bevy::tasks::futures_lite::Stream;
use futures_sink::Sink;
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::borrow::Borrow;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
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

#[derive(Debug, Clone)]
pub enum MaybeArc<'a, T: ?Sized> {
    Borrowed(&'a T),
    Owned(Arc<T>),
}
impl<'a, T: ?Sized> From<&'a T> for MaybeArc<'a, T> {
    fn from(value: &'a T) -> Self {
        Self::Borrowed(value)
    }
}
impl<T: ?Sized> From<Arc<T>> for MaybeArc<'_, T> {
    fn from(value: Arc<T>) -> Self {
        Self::Owned(value)
    }
}
impl<T: ?Sized> Deref for MaybeArc<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Borrowed(r) => r,
            Self::Owned(o) => o,
        }
    }
}
impl<T: ?Sized> AsRef<T> for MaybeArc<'_, T> {
    fn as_ref(&self) -> &T {
        self
    }
}
impl<T: ?Sized> Borrow<T> for MaybeArc<'_, T> {
    fn borrow(&self) -> &T {
        self
    }
}
impl<T: ?Sized + PartialEq> PartialEq for MaybeArc<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
    #[allow(clippy::partialeq_ne_impl)]
    fn ne(&self, other: &Self) -> bool {
        **self != **other
    }
}
impl<T: ?Sized + PartialOrd> PartialOrd for MaybeArc<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(other)
    }
}
impl<T: ?Sized + Eq> Eq for MaybeArc<'_, T> {}
impl<T: ?Sized + Ord> Ord for MaybeArc<'_, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (**self).cmp(other)
    }
}
impl<T: ?Sized + Hash> Hash for MaybeArc<'_, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct UpdateStates;
impl Command for UpdateStates {
    fn apply(self, world: &mut World) {
        let _ = world.try_run_schedule(StateTransition);
    }
}

/// Workaround for https://github.com/rust-lang/rust/issues/64552
#[derive(Debug, Clone, Copy)]
pub struct UnsafeAssertSend<T>(T);
impl<T> UnsafeAssertSend<T> {
    /// Create a new version of this. T must be send, even if the compiler refuses to verify it.
    #[allow(clippy::missing_safety_doc)]
    pub const unsafe fn new(inner: T) -> Self {
        Self(inner)
    }
    pub fn into_inner(self) -> T {
        self.0
    }
}
impl<'a, T: Sync> UnsafeAssertSend<&'a T> {
    /// This is safe because `T: Sync`. The auto bound messes this up in async functions
    pub const fn from_ref(inner: &'a T) -> Self {
        Self(inner)
    }
}
unsafe impl<T> Send for UnsafeAssertSend<T> {}
impl<T> Deref for UnsafeAssertSend<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T> DerefMut for UnsafeAssertSend<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
