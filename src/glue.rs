use bevy::prelude::*;
use factor_client::chunks::ChunkInterest;
use factor_client::core_ui::ClientState;
use factor_client::player::PlayerPos;
use factor_common::PlayerId;
use factor_server::player::{PlayerLoaded, PlayerRequest, PlayerState};
use factor_server::utils::database as redb;
use std::error::Error;
use unsize::*;

pub fn after_loaded(
    res: In<Result<(), redb::Error>>,
    mut commands: Commands,
    mut next_state: ResMut<NextState<ClientState>>,
) {
    if let Err(err) = res.0 {
        next_state.set(ClientState::LoadingFailed {
            cause: triomphe::Arc::new(err).unsize(Coercion!(to dyn Error + Send + Sync)),
            return_to_sp: true,
        });
    } else {
        commands.observe(player_loaded);
        commands.trigger(PlayerRequest(PlayerId::DEFAULT));
        next_state.set(ClientState::WorldLoading);
    }
}
pub fn player_loaded(
    trigger: Trigger<PlayerLoaded>,
    observer: Query<Entity, With<Observer<PlayerLoaded, ()>>>,
    player: Query<&PlayerState>,
    mut commands: Commands,
    mut next_state: ResMut<NextState<ClientState>>,
) {
    debug_assert_eq!(trigger.event().id, PlayerId::DEFAULT);
    commands.entity(observer.single()).despawn();
    if let Err(err) = &trigger.event().res {
        next_state.set(ClientState::LoadingFailed {
            cause: err.clone(),
            return_to_sp: true,
        });
    } else {
        let mut player_iter = player.iter().filter(|p| p.id == PlayerId::DEFAULT);
        let player = player_iter.next().unwrap();
        debug_assert!(player_iter.next().is_none());
        commands.init_resource::<ChunkInterest>();
        commands.insert_resource(PlayerPos {
            chunk: player.data.chunk,
            position: player.data.pos,
        });
        next_state.set(ClientState::Running);
    }
}
