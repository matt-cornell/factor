use crate::player::{PlayerData, PlayerIdKey};
use crate::terrain::climate::ClimateCell;
use crate::utils::db_value::{ErasedValue, PostcardValue};
use redb::{Key, TableDefinition, Value};

pub const PLAYERS: TableDefinition<PlayerIdKey, PostcardValue<Option<PlayerData>>> =
    TableDefinition::new("players");

pub const MISC_DATA: TableDefinition<DataEntry, ErasedValue> = TableDefinition::new("misc");

pub const TERRAIN: TableDefinition<u64, ClimateCell> = TableDefinition::new("climate");

pub const HIGH_VALUE_NOISE: TableDefinition<NoiseLocation, f32> =
    TableDefinition::new("high-value-noise");

pub const HIGH_GRAD_NOISE: TableDefinition<NoiseLocation, [f32; 2]> =
    TableDefinition::new("high-octave-noise");

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NoiseLocation {
    pub layer: u8,
    pub cell: u64,
}
impl redb::Value for NoiseLocation {
    type AsBytes<'a> = [u8; 9];
    type SelfType<'a> = Self;

    fn as_bytes<'a, 'b: 'a>(value: &'a Self::SelfType<'b>) -> Self::AsBytes<'a>
    where
        Self: 'a,
        Self: 'b,
    {
        let mut out = [0u8; 9];
        out[0] = value.layer;
        out[1..].copy_from_slice(&value.cell.to_le_bytes());
        out
    }
    fn from_bytes<'a>(data: &'a [u8]) -> Self::SelfType<'a>
    where
        Self: 'a,
    {
        let mut buf = [0u8; 8];
        buf.copy_from_slice(&data[1..9]);
        Self {
            layer: buf[0],
            cell: u64::from_le_bytes(buf),
        }
    }
    fn fixed_width() -> Option<usize> {
        Some(9)
    }
    fn type_name() -> redb::TypeName {
        redb::TypeName::new("factor::NoiseLocation")
    }
}
impl redb::Key for NoiseLocation {
    fn compare(data1: &[u8], data2: &[u8]) -> std::cmp::Ordering {
        data1[..9].cmp(&data2[..9])
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum DataEntry {
    WorldConfig,
    Playtime,
}
impl Value for DataEntry {
    type AsBytes<'a> = [u8; 1];
    type SelfType<'a> = Self;
    fn as_bytes<'a, 'b: 'a>(value: &'a Self::SelfType<'b>) -> Self::AsBytes<'a>
    where
        Self: 'a,
        Self: 'b,
    {
        [*value as u8]
    }
    fn from_bytes<'a>(data: &'a [u8]) -> Self::SelfType<'a>
    where
        Self: 'a,
    {
        match data[0] {
            0 => Self::WorldConfig,
            1 => Self::Playtime,
            n => unreachable!("Invalid determinant for DataEntry: {n}"),
        }
    }
    fn fixed_width() -> Option<usize> {
        Some(1)
    }
    fn type_name() -> redb::TypeName {
        redb::TypeName::new("factor::DataEntry")
    }
}
impl Key for DataEntry {
    fn compare(data1: &[u8], data2: &[u8]) -> std::cmp::Ordering {
        data1[0].cmp(&data2[0])
    }
}
