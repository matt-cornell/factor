#![feature(slice_split_once)]
use serde::{Deserialize, Serialize};
use std::fmt::{self, Debug, Display, Formatter};

pub mod cell;
pub mod coords;
pub mod glue;
pub mod healpix;
pub mod noise;
pub mod rcu;
pub mod util;

pub const PLANET_RADIUS: f64 = 1_000_000.0;

/// A wrapper around a player ID hash.
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct PlayerId([u8; 32]);
impl PlayerId {
    /// Zero hash for the default player.
    pub const DEFAULT: Self = Self([0; 32]);
    /// Create this from a raw hash. This shouldn't be used much, favoring [`from_id`].
    #[inline(always)]
    pub fn from_raw(hash: [u8; 32]) -> Self {
        Self(hash)
    }
    /// Create this by hashing an input ID.
    pub fn from_id<I: AsRef<[u8]>>(id: I) -> Self {
        let hashed = hmac_sha256::Hash::hash(id.as_ref());
        if hashed == [0u8; 32] {
            Self([255; 32])
        } else {
            Self(hashed)
        }
    }
    pub fn as_bytes(&self) -> [u8; 32] {
        self.0
    }
}
impl Debug for PlayerId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut buf = [0; 64];
        let _ = hex::encode_to_slice(self.0, &mut buf);
        let as_hex = std::str::from_utf8(&buf).unwrap();
        Debug::fmt(as_hex, f)
    }
}
impl Display for PlayerId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut buf = [0; 64];
        let _ = hex::encode_to_slice(self.0, &mut buf);
        let as_hex = std::str::from_utf8(&buf).unwrap();
        f.write_str(as_hex)
    }
}
impl Serialize for PlayerId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if serializer.is_human_readable() {
            hex::serde::serialize(self.0, serializer)
        } else {
            serializer.serialize_bytes(&self.0)
        }
    }
}
impl<'de> Deserialize<'de> for PlayerId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::*;
        struct PlayerIdVisitor;
        impl Visitor<'_> for PlayerIdVisitor {
            type Value = PlayerId;
            fn expecting(&self, formatter: &mut Formatter) -> fmt::Result {
                formatter.write_str("a player identifier")
            }
            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match v.len() {
                    32 => Ok(PlayerId(<[u8; 32]>::try_from(v).unwrap())),
                    64 => {
                        let mut buf = [0; 32];
                        if hex::decode_to_slice(v, &mut buf).is_err() {
                            Err(E::invalid_value(Unexpected::Bytes(v), &self))
                        } else {
                            Ok(PlayerId(buf))
                        }
                    }
                    _ => Err(E::invalid_value(Unexpected::Bytes(v), &self)),
                }
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match v.len() {
                    32 => Ok(PlayerId(<[u8; 32]>::try_from(v.as_bytes()).unwrap())),
                    64 => {
                        let mut buf = [0; 32];
                        if hex::decode_to_slice(v, &mut buf).is_err() {
                            Err(E::invalid_value(Unexpected::Str(v), &self))
                        } else {
                            Ok(PlayerId(buf))
                        }
                    }
                    _ => Err(E::invalid_value(Unexpected::Str(v), &self)),
                }
            }
        }
        deserializer.deserialize_bytes(PlayerIdVisitor)
    }
}
