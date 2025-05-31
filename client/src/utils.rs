use bevy::ecs::system::Command;
use bevy::prelude::*;
use std::future::Future;
use std::pin::Pin;
use std::sync::OnceLock;
use std::task::{Context, Poll};

/// A component that marks an entity as despawnable when we're done with it.
///
/// Entities can either be handled by multiplayer code, in which case they can be despawned immediately, or they can be shared with the server, in which case we just remove our components
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Component)]
pub struct Despawnable;

/// A bundle to mark all of the stuff that we have in a chunk that the server doesn't need.
#[derive(Debug, Clone, Bundle)]
pub struct ClientChunkBundle {
    mesh: Mesh3d,
    material: MeshMaterial3d<StandardMaterial>,
}

/// A command that will either despawn an entity or remove its client-only components.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Cleanup(pub Entity);
impl Command for Cleanup {
    fn apply(self, world: &mut World) {
        let Ok(mut e) = world.get_entity_mut(self.0) else {
            return;
        };
        if e.contains::<Despawnable>() {
            e.despawn();
        } else {
            e.remove::<ClientChunkBundle>();
        }
    }
}

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
