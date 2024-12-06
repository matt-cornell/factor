use bevy::log::*;
use serde::de::DeserializeOwned;
use serde::ser::Serialize;
use serde::Deserialize;
use std::borrow::Cow;
use std::fmt::Debug;
use std::marker::PhantomData;

/// A marker for table definitions that a value should be serialized and deserialzied with postcard
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PostcardValue<T>(PhantomData<T>);
impl<T> PostcardValue<T> {
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<T: Debug + Default + Serialize + DeserializeOwned + 'static> redb::Value for PostcardValue<T> {
    type AsBytes<'a> = Vec<u8>;
    type SelfType<'a> = T;

    fn fixed_width() -> Option<usize> {
        None
    }
    fn as_bytes<'a, 'b: 'a>(value: &'a Self::SelfType<'b>) -> Self::AsBytes<'a>
    where
        Self: 'a,
        Self: 'b,
    {
        postcard::to_stdvec(value).unwrap_or_else(|err| {
            error!(%err, "Error serializing value");
            trace!(?value, "Problem value");
            Vec::new()
        })
    }
    fn from_bytes<'a>(data: &'a [u8]) -> Self::SelfType<'a>
    where
        Self: 'a,
    {
        postcard::from_bytes(data).unwrap_or_else(|err| {
            error!(%err, "Error deserializing value");
            T::default()
        })
    }
    fn type_name() -> redb::TypeName {
        redb::TypeName::new(&format!("Postcard<{}>", std::any::type_name::<T>()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ErasedValue<'a> {
    pub bytes: Cow<'a, [u8]>,
}
impl ErasedValue<'_> {
    pub fn new<T: Serialize>(value: &T) -> Self {
        Self {
            bytes: postcard::to_stdvec(value).map_or_else(
                |err| {
                    error!(%err, "Error serializing value");
                    Cow::Borrowed(&[] as _)
                },
                Cow::Owned,
            ),
        }
    }
    pub fn get<'a, T: Deserialize<'a>>(&'a self) -> Result<T, postcard::Error> {
        postcard::from_bytes(&self.bytes).inspect_err(|err| {
            error!(%err, "Error deserializing value");
        })
    }
}
impl redb::Value for ErasedValue<'static> {
    type AsBytes<'a> = &'a [u8];
    type SelfType<'a> = ErasedValue<'a>;

    fn fixed_width() -> Option<usize> {
        None
    }
    fn as_bytes<'a, 'b: 'a>(value: &'a Self::SelfType<'b>) -> Self::AsBytes<'a>
    where
        Self: 'a,
        Self: 'b,
    {
        &value.bytes
    }
    fn from_bytes<'a>(data: &'a [u8]) -> Self::SelfType<'a>
    where
        Self: 'a,
    {
        ErasedValue {
            bytes: Cow::Borrowed(data),
        }
    }
    fn type_name() -> redb::TypeName {
        redb::TypeName::new("ErasedValue")
    }
}
