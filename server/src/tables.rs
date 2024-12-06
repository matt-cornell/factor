use crate::utils::db_value::{ErasedValue, PostcardValue};
use factor_common::glue::PlayerData;
use redb::{Key, TableDefinition, Value};

pub const PLAYERS: TableDefinition<String, PostcardValue<Option<PlayerData>>> =
    TableDefinition::new("players");

pub const MISC_DATA: TableDefinition<DataEntry, ErasedValue> = TableDefinition::new("misc");

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
        redb::TypeName::new("DataEntry")
    }
}
impl Key for DataEntry {
    fn compare(data1: &[u8], data2: &[u8]) -> std::cmp::Ordering {
        data1.cmp(data2)
    }
}
