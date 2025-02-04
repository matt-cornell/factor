//! This file re-exports all of `redb`, along with a wrapper type that is a `Resource`.

use bevy::prelude::Resource;
use std::ops::Deref;
use triomphe::Arc;

pub use redb::{Database as InnerDatabase, *};

/// A wrapper around a `Database` that can be used as a `Resource`.
#[derive(Debug, Clone, Resource)]
pub struct Database {
    inner: Arc<InnerDatabase>,
}
impl Database {
    /// Create a temporary database in memory.
    pub fn temporary() -> Self {
        Self {
            inner: Arc::new(InnerDatabase::builder()
                .create_with_backend(backends::InMemoryBackend::new())
                .unwrap()),
        }
    }
    /// Create a persistent database with a given name.
    pub fn persistent(name: String) -> Result<Self, DatabaseError> {
        Ok(Self {
            inner: Arc::new(InnerDatabase::builder()
                .create_with_backend(crate::storage::PersistentBackend::new(name)?)
                .inspect_err(|err| bevy::log::error!(%err, "Error opening database"))?),
        })
    }
    /// Get the inner database.
    pub fn into_inner(self) -> Arc<InnerDatabase> {
        self.inner
    }
    pub fn get_mut(&mut self) -> Option<&mut InnerDatabase> {
        Arc::get_mut(&mut self.inner)
    }
}
impl Deref for Database {
    type Target = InnerDatabase;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl From<InnerDatabase> for Database {
    fn from(value: InnerDatabase) -> Self {
        Self {
            inner: Arc::new(value)
        }
    }
}
impl From<Arc<InnerDatabase>> for Database {
    fn from(value: Arc<InnerDatabase>) -> Self {
        Self {
            inner: value
        }
    }
}