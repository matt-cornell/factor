use bevy::prelude::*;
use factor_client::core_ui::ClientState;
use factor_server::utils::database as redb;
use std::error::Error;
use unsize::*;

pub fn after_loaded(
    res: In<Result<(), redb::Error>>,
    mut next_state: ResMut<NextState<ClientState>>,
) {
    if let Err(err) = res.0 {
        next_state.set(ClientState::LoadingFailed {
            cause: triomphe::Arc::new(err).unsize(Coercion!(to dyn Error + Send + Sync)),
            return_to_sp: true,
        });
    } else {
        next_state.set(ClientState::Running);
    }
}
