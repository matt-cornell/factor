use crate::coords::{get_absolute, LonLat};
use bevy::ecs::archetype::Archetype;
use bevy::ecs::component::{ComponentId, Components, Tick};
use bevy::ecs::query::{FilteredAccess, QueryFilter, WorldQuery};
use bevy::ecs::storage::{Table, TableRow};
use bevy::prelude::*;
use serde::{Deserialize, Serialize};
use std::fmt::{self, Debug, Display, Formatter};
use crate::PLANET_RADIUS;

/// A wrapper around a player ID hash.
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Component)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Component)]
pub struct ChunkId(pub u64);

#[derive(Debug, Clone, Component)]
pub struct ChunkInterest {
    pub chunks: tinyset::SetU64,
}
impl ChunkInterest {
    pub fn new() -> Self {
        Self {
            chunks: tinyset::SetU64::new(),
        }
    }
}
impl Default for ChunkInterest {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Serialize, Deserialize, Component)]
pub struct Position {
    /// Depth 12 cell that this is contained in
    pub frame: u64,
    /// Position relative to the center of the frame
    pub pos: Vec3,
    pub rot: Quat,
}
impl Position {
    pub fn get_isometry(&self) -> Isometry3d {
        Isometry3d::new(self.pos, self.rot)
    }
    pub const fn get_transform(&self) -> Transform {
        Transform {
            translation: self.pos,
            rotation: self.rot,
            scale: Vec3::splat(1.0),
        }
    }
    pub fn get_absolute(&self) -> LonLat {
        get_absolute(
            crate::healpix::Layer::new(12).center(self.frame),
            self.pos.xz().as_dvec2() / PLANET_RADIUS,
        )
    }
    /// Get the containing depth 16 frame, i.e. the chunk
    pub fn get_chunk(&self) -> u64 {
        crate::healpix::Layer::new(16).hash(self.get_absolute()) // TODO: use inverse bilinear
    }
}

type PIRef = &'static PlayerId;

/// `QueryFilter` to only allow the default player's data
#[derive(Debug, Default, Clone, Copy)]
pub struct DefaultPlayer;
unsafe impl WorldQuery for DefaultPlayer {
    type Fetch<'a> = <PIRef as WorldQuery>::Fetch<'a>;
    type Item<'a> = ();
    type State = <PIRef as WorldQuery>::State;

    const IS_DENSE: bool = false;

    fn shrink<'wlong: 'wshort, 'wshort>(_item: Self::Item<'wlong>) -> Self::Item<'wshort> {}
    fn shrink_fetch<'wlong: 'wshort, 'wshort>(fetch: Self::Fetch<'wlong>) -> Self::Fetch<'wshort> {
        PIRef::shrink_fetch(fetch)
    }
    fn matches_component_set(
        state: &Self::State,
        set_contains_id: &impl Fn(ComponentId) -> bool,
    ) -> bool {
        PIRef::matches_component_set(state, set_contains_id)
    }
    unsafe fn set_archetype<'w>(
        fetch: &mut Self::Fetch<'w>,
        state: &Self::State,
        archetype: &'w Archetype,
        table: &'w Table,
    ) {
        PIRef::set_archetype(fetch, state, archetype, table);
    }
    fn set_access(state: &mut Self::State, access: &FilteredAccess<ComponentId>) {
        PIRef::set_access(state, access);
    }
    unsafe fn set_table<'w>(fetch: &mut Self::Fetch<'w>, state: &Self::State, table: &'w Table) {
        PIRef::set_table(fetch, state, table);
    }
    fn update_component_access(state: &Self::State, access: &mut FilteredAccess<ComponentId>) {
        PIRef::update_component_access(state, access);
    }

    unsafe fn init_fetch<'w>(
        world: bevy::ecs::world::unsafe_world_cell::UnsafeWorldCell<'w>,
        state: &Self::State,
        last_run: Tick,
        this_run: Tick,
    ) -> Self::Fetch<'w> {
        PIRef::init_fetch(world, state, last_run, this_run)
    }

    fn init_state(world: &mut World) -> Self::State {
        PIRef::init_state(world)
    }

    fn get_state(components: &Components) -> Option<Self::State> {
        PIRef::get_state(components)
    }

    unsafe fn fetch<'w>(
        fetch: &mut Self::Fetch<'w>,
        entity: Entity,
        table_row: TableRow,
    ) -> Self::Item<'w> {
        PIRef::fetch(fetch, entity, table_row);
    }
}
unsafe impl QueryFilter for DefaultPlayer {
    const IS_ARCHETYPAL: bool = false;
    unsafe fn filter_fetch(
        fetch: &mut Self::Fetch<'_>,
        entity: Entity,
        table_row: TableRow,
    ) -> bool {
        *PIRef::fetch(fetch, entity, table_row) == PlayerId::DEFAULT
    }
}
