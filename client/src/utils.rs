use std::future::Future;
use std::pin::Pin;
use std::sync::OnceLock;
use std::task::{Context, Poll};

#[derive(Debug)]
pub struct GetOnceLock<'a, T> {
    lock: &'a OnceLock<T>,
}
impl<T> Clone for GetOnceLock<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for GetOnceLock<'_, T> {}
impl<'a, T> GetOnceLock<'a, T> {
    pub fn new(lock: &'a OnceLock<T>) -> Self {
        Self { lock }
    }
}
impl<'a, T> Future for GetOnceLock<'a, T> {
    type Output = &'a T;
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if let Some(val) = self.lock.get() {
            Poll::Ready(val)
        } else {
            cx.waker().wake_by_ref();
            Poll::Pending
        }
    }
}
