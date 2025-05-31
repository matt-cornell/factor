//! This file re-exports all of `redb`, along with a wrapper type that is a `Resource`.

use bevy::prelude::Resource;
use std::ops::Deref;
use triomphe::Arc;

pub use redb::{
    backends, Database as InnerDatabase, DatabaseError, Error, Key, ReadOnlyTable, ReadTransaction,
    ReadableTable, ReadableTableMetadata, StorageError, Table, TableDefinition, TableError,
    TypeName, Value, WriteTransaction,
};

/// A wrapper around a `Database` that can be used as a `Resource`.
#[derive(Debug, Clone, Resource)]
pub struct Database {
    inner: Arc<InnerDatabase>,
}
impl Database {
    /// Create a temporary database in memory.
    pub fn temporary() -> Self {
        Self {
            inner: Arc::new(
                InnerDatabase::builder()
                    .create_with_backend(backends::InMemoryBackend::new())
                    .unwrap(),
            ),
        }
    }
    /// Create a persistent database with a given name.
    pub fn persistent(name: String) -> Result<Self, DatabaseError> {
        Ok(Self {
            inner: Arc::new(
                InnerDatabase::builder()
                    .create_with_backend(crate::storage::PersistentBackend::new(name)?)
                    .inspect_err(|err| bevy::log::error!(%err, "Error opening database"))?,
            ),
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
            inner: Arc::new(value),
        }
    }
}
impl From<Arc<InnerDatabase>> for Database {
    fn from(value: Arc<InnerDatabase>) -> Self {
        Self { inner: value }
    }
}

/// Extension trait to convert a `Result<T, TableError>` to a `Result<Option<T>, TableError>` in a way that doesn't treat `TableError::TableDoesNotExist` as an error.
pub trait TableResultExt {
    type Ok;
    type Err;

    fn if_exists(self) -> Result<Option<Self::Ok>, Self::Err>;
}
impl<T> TableResultExt for Result<T, TableError> {
    type Ok = T;
    type Err = TableError;

    fn if_exists(self) -> Result<Option<T>, TableError> {
        self.map(Some).or_else(|err| {
            matches!(err, TableError::TableDoesNotExist(_))
                .then_some(None)
                .ok_or(err)
        })
    }
}
impl<T> TableResultExt for Result<T, Error> {
    type Ok = T;
    type Err = Error;

    fn if_exists(self) -> Result<Option<T>, Error> {
        self.map(Some).or_else(|err| {
            matches!(err, Error::TableDoesNotExist(_))
                .then_some(None)
                .ok_or(err)
        })
    }
}
