//! This file re-exports all of `redb`, along with a wrapper type that is a `Resource`.

use bevy::prelude::Resource;
use std::ops::{Deref, DerefMut};

pub use redb::{Database as InnerDatabase, *};

/// A wrapper around a `Database` that can be used as a `Resource`.
#[derive(Debug, Resource)]
pub struct Database {
    inner: InnerDatabase,
}
impl Database {
    /// Create a temporary database in memory.
    pub fn temporary() -> Self {
        Self {
            inner: InnerDatabase::builder()
                .create_with_backend(backends::InMemoryBackend::new())
                .unwrap(),
        }
    }
    /// Create a persistent database with a given name.
    pub fn persistent(name: String) -> Result<Self, DatabaseError> {
        Ok(Self {
            inner: InnerDatabase::builder()
                .create_with_backend(crate::storage::PersistentBackend::new(name)?)?,
        })
    }
    /// Get the inner database.
    pub fn into_inner(self) -> InnerDatabase {
        self.inner
    }
}
impl Deref for Database {
    type Target = InnerDatabase;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl DerefMut for Database {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
