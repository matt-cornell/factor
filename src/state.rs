use bevy::prelude::*;
use factor_client::core_ui::ClientState;

#[derive(Debug, Clone, PartialEq, Eq, Hash, States)]
pub enum SingleplayerState {
    Base(ClientState),
    /// World creation, with a starting name.
    ///
    /// If this is `None`, then we're using a temporary world.
    WorldCreation(Option<String>),
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash, SubStates)]
#[source(SingleplayerState = SingleplayerState::WorldCreation(_))]
pub struct CreatingWorld;

impl From<ClientState> for SingleplayerState {
    #[inline(always)]
    fn from(value: ClientState) -> Self {
        Self::Base(value)
    }
}

/// Link the `ClientState` with the `SingleplayerState`.
pub fn link_states(
    mut base_evt: EventReader<StateTransitionEvent<ClientState>>,
    mut sp_evt: EventReader<StateTransitionEvent<SingleplayerState>>,
    mut next_base: ResMut<NextState<ClientState>>,
    mut next_sp: ResMut<NextState<SingleplayerState>>,
    old_base: Res<State<ClientState>>,
    old_sp: Res<State<SingleplayerState>>,
) {
    for evt in base_evt.read() {
        if let Some(st) = evt.entered {
            let next = SingleplayerState::Base(st);
            if st != ClientState::Other("sp") && **old_sp != next {
                next_sp.set(next);
            }
        }
    }
    for evt in sp_evt.read() {
        if let Some(st) = &evt.entered {
            let next = if let &SingleplayerState::Base(next) = st {
                next
            } else {
                ClientState::Other("sp")
            };
            if **old_base != next {
                next_base.set(next);
            }
        }
    }
}
