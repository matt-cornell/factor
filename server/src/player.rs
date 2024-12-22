use crate::tables::PLAYERS;
use crate::terrain::climate::ClimateFlags;
use crate::utils::database::{self as redb, Database};
use crate::utils::random_point_in_quadrilateral;
use crate::ClimateData;
use bevy::ecs::archetype::Archetype;
use bevy::ecs::component::{ComponentId, Components, Tick};
use bevy::ecs::query::{FilteredAccess, QueryFilter, WorldQuery};
use bevy::ecs::storage::{Table, TableRow};
use bevy::prelude::*;
use factor_common::PlayerId;
use itertools::Itertools;
use rand::prelude::*;
use serde::{Deserialize, Serialize};
use std::error::Error;
use std::f32::consts::TAU;
use triomphe::Arc;
use unsize::*;

#[derive(Debug, Default, Clone, Copy)]
pub struct PlayerIdKey;
impl redb::Value for PlayerIdKey {
    type AsBytes<'a> = [u8; 32];
    type SelfType<'a> = PlayerId;

    fn as_bytes<'a, 'b: 'a>(value: &'a Self::SelfType<'b>) -> Self::AsBytes<'a>
    where
        Self: 'a,
        Self: 'b,
    {
        value.as_bytes()
    }
    fn from_bytes<'a>(data: &'a [u8]) -> Self::SelfType<'a>
    where
        Self: 'a,
    {
        PlayerId::from_raw(data[..32].try_into().unwrap())
    }
    fn fixed_width() -> Option<usize> {
        Some(32)
    }
    fn type_name() -> redb::TypeName {
        redb::TypeName::new("factor::PlayerId")
    }
}
impl redb::Key for PlayerIdKey {
    fn compare(data1: &[u8], data2: &[u8]) -> std::cmp::Ordering {
        data1[..32].cmp(&data2[..32])
    }
}

/// Data about the player that gets serialized and deserialzied.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlayerData {
    pub chunk: u64,
    pub pos: Vec3,
    pub rot: Quat,
}

/// State for a player, with their data and actual
#[derive(Debug, Clone, Component)]
pub struct PlayerState {
    pub id: PlayerId,
    pub interest: tinyset::SetU64,
    pub data: PlayerData,
}

/// A request for a player to be loaded.
#[derive(Debug, Clone, Copy, PartialEq, Event)]
pub struct PlayerRequest(pub PlayerId);

/// A player has been loaded.
#[derive(Debug, Clone, Event)]
pub struct PlayerLoaded {
    pub id: PlayerId,
    pub res: Result<(), Arc<dyn Error + Send + Sync>>,
}

pub fn load_player(
    trigger: Trigger<PlayerRequest>,
    db: Res<Database>,
    world: Res<ClimateData>,
    mut commands: Commands,
) {
    let id = trigger.event().0;
    let res: Result<_, redb::Error> = try {
        'load: {
            let txn = db.begin_read()?;
            match txn.open_table(PLAYERS) {
                Ok(table) => {
                    if let Some(player) = table.get(id)? {
                        if let Some(data) = player.value() {
                            break 'load data;
                        }
                    }
                }
                Err(redb::TableError::TableDoesNotExist(_)) => {}
                Err(err) => Err(err)?,
            }
            txn.close()?;
            let txn = db.begin_write()?;
            let mut table = txn.open_table(PLAYERS)?;
            let mut rng = thread_rng();
            let clim_cell = *world
                .cells
                .iter()
                .positions(|c| !c.flags.contains(ClimateFlags::OCEAN)) // TODO: better starting conditions
                .map(|i| i as u64)
                .collect::<Vec<_>>()
                .choose(&mut rng)
                .unwrap();
            let chunk = if let Some(depth_diff) = 16u8.checked_sub(world.depth) {
                let additional = rng.gen_range(0..(1 << depth_diff));
                clim_cell << (depth_diff * 2) | additional
            } else {
                clim_cell >> ((world.depth - 16) * 2)
            };
            let coords =
                random_point_in_quadrilateral(factor_common::cell::corners_of(16, chunk), &mut rng);
            let data = PlayerData {
                chunk,
                pos: coords.extend(0.0).xzy(),
                rot: Quat::from_rotation_y(rng.gen_range(0.0..=TAU)),
            };
            let opt = Some(data);
            table.insert(id, &opt)?;
            opt.unwrap()
        }
    };
    if let Err(err) = &res {
        error!(%id, %err, "Error loading player");
    }
    let res = match res {
        Ok(data) => {
            commands.spawn(PlayerState {
                id,
                data,
                interest: tinyset::SetU64::new(),
            });
            Ok(())
        }
        Err(err) => Err(Arc::new(err).unsize(Coercion!(to dyn Error + Send + Sync))),
    };
    commands.trigger(PlayerLoaded { id, res });
}

type PSRef = &'static PlayerState;

/// `QueryFilter` to only allow the default player's data
#[derive(Debug, Default, Clone, Copy)]
pub struct DefaultPlayer;
unsafe impl WorldQuery for DefaultPlayer {
    type Fetch<'a> = <PSRef as WorldQuery>::Fetch<'a>;
    type Item<'a> = ();
    type State = <PSRef as WorldQuery>::State;

    const IS_DENSE: bool = false;

    fn shrink<'wlong: 'wshort, 'wshort>(_item: Self::Item<'wlong>) -> Self::Item<'wshort> {}
    fn shrink_fetch<'wlong: 'wshort, 'wshort>(fetch: Self::Fetch<'wlong>) -> Self::Fetch<'wshort> {
        PSRef::shrink_fetch(fetch)
    }
    fn matches_component_set(
        state: &Self::State,
        set_contains_id: &impl Fn(ComponentId) -> bool,
    ) -> bool {
        PSRef::matches_component_set(state, set_contains_id)
    }
    unsafe fn set_archetype<'w>(
        fetch: &mut Self::Fetch<'w>,
        state: &Self::State,
        archetype: &'w Archetype,
        table: &'w Table,
    ) {
        PSRef::set_archetype(fetch, state, archetype, table);
    }
    fn set_access(state: &mut Self::State, access: &FilteredAccess<ComponentId>) {
        PSRef::set_access(state, access);
    }
    unsafe fn set_table<'w>(fetch: &mut Self::Fetch<'w>, state: &Self::State, table: &'w Table) {
        PSRef::set_table(fetch, state, table);
    }
    fn update_component_access(state: &Self::State, access: &mut FilteredAccess<ComponentId>) {
        PSRef::update_component_access(state, access);
    }

    unsafe fn init_fetch<'w>(
        world: bevy::ecs::world::unsafe_world_cell::UnsafeWorldCell<'w>,
        state: &Self::State,
        last_run: Tick,
        this_run: Tick,
    ) -> Self::Fetch<'w> {
        PSRef::init_fetch(world, state, last_run, this_run)
    }

    fn init_state(world: &mut World) -> Self::State {
        PSRef::init_state(world)
    }

    fn get_state(components: &Components) -> Option<Self::State> {
        PSRef::get_state(components)
    }

    unsafe fn fetch<'w>(
        fetch: &mut Self::Fetch<'w>,
        entity: Entity,
        table_row: TableRow,
    ) -> Self::Item<'w> {
        PSRef::fetch(fetch, entity, table_row);
    }
}
unsafe impl QueryFilter for DefaultPlayer {
    const IS_ARCHETYPAL: bool = false;
    unsafe fn filter_fetch(
        fetch: &mut Self::Fetch<'_>,
        entity: Entity,
        table_row: TableRow,
    ) -> bool {
        PSRef::fetch(fetch, entity, table_row).id == PlayerId::DEFAULT
    }
}
