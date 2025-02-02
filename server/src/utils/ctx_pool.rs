use stable_deref_trait::StableDeref;
use std::fmt::{self, Debug, Formatter};
use std::thread::JoinHandle;

/// A pointer that can be sent between threads
struct SendablePointer<T: ?Sized>(*const T);
impl<T: ?Sized> SendablePointer<T> {
    unsafe fn deref<'a>(&self) -> &'a T {
        &*self.0
    }
}
unsafe impl<T: Sync + ?Sized> Send for SendablePointer<T> {}
impl<T: ?Sized> Clone for SendablePointer<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T: ?Sized> Copy for SendablePointer<T> {}

/// A thread pool with a context made available to them.
///
/// This is similar to using scoped threads or [`Arc`](std::thread::Arc), but it has ownership over the value and ensures that all of the threads terminate before it can be dropped.
#[derive(Default)]
pub struct CtxPool<T> {
    context: T,
    threads: Vec<JoinHandle<()>>,
}
impl<T> CtxPool<T> {
    /// Create an empty thread pool
    pub const fn empty(context: T) -> Self {
        Self {
            context,
            threads: Vec::new(),
        }
    }
    /// Get the context shared between the threads.
    pub fn ctx(&self) -> &T {
        &self.context
    }
    pub fn threads(&self) -> &[JoinHandle<()>] {
        &self.threads
    }
}
impl<T: StableDeref<Target: Sync>> CtxPool<T> {
    /// Create a new thread pool with the given context and callback factory.
    ///
    /// This takes a factory callback to create the callbacks, to handle cloning
    pub fn new<F: Fn(&T::Target, usize) + Clone + Send + 'static>(
        context: T,
        num_threads: usize,
        mut factory: impl FnMut() -> F,
    ) -> Self {
        let ptr = SendablePointer(T::deref(&context));
        let threads = (0..num_threads)
            .filter_map(|i| unsafe {
                let func = factory();
                match std::thread::Builder::new().spawn_unchecked(move || func(ptr.deref(), i)) {
                    Ok(handle) => Some(handle),
                    Err(err) => {
                        bevy::log::error!(%err, "Failed to spawn thread");
                        None
                    }
                }
            })
            .collect();
        Self { context, threads }
    }
    /// Add more threads to this thread pool that share the context.
    ///
    /// This takes a factory callback to create the callbacks, to handle cloning
    pub fn add<F: Fn(&T::Target, usize) + Clone + Send + 'static>(
        &mut self,
        num_threads: usize,
        mut factory: impl FnMut() -> F,
    ) -> std::io::Result<()> {
        let l = self.threads.len();
        let ptr = SendablePointer(T::deref(&self.context));
        self.threads.reserve(num_threads);
        (l..(l + num_threads)).try_for_each(|i| {
            let func = factory();
            let handle = unsafe {
                std::thread::Builder::new().spawn_unchecked(move || func(ptr.deref(), i))?
            };
            self.threads.push(handle);
            Ok(())
        })
    }
}
impl<T: Debug> Debug for CtxPool<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("CtxPool")
            .field("context", &self.context)
            .field("threads", &self.threads.len())
            .finish_non_exhaustive()
    }
}
impl<T> Drop for CtxPool<T> {
    fn drop(&mut self) {
        let mut panic = None;
        for handle in self.threads.drain(..) {
            if let Err(err) = handle.join() {
                panic = Some(err);
            }
        }
        if let Some(panic) = panic {
            std::panic::resume_unwind(panic);
        }
    }
}
