use bevy::prelude::*;
use factor_client::core_ui::ClientState;

#[derive(Debug, Clone, PartialEq, Eq, Hash, States)]
pub enum SingleplayerState {
    Base(ClientState),
    /// World creation, with a starting name.
    ///
    /// If this is `None`, then we're using a temporary world.
    WorldCreation(Option<String>),
    /// This is the state that we enter when creating a world.
    CreatingWorld,
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
    if let Some(evt) = base_evt.read().filter(|evt| evt.entered.is_some()).last() {
        let Some(st) = &evt.entered else {
            unreachable!()
        };
        let next = SingleplayerState::Base(st.clone());
        if *st != ClientState::Other("sp") && **old_sp != next {
            info!(?next, "setting sp state");
            next_sp.set(next);
        }
        sp_evt.clear();
    } else if let Some(evt) = sp_evt.read().filter(|evt| evt.entered.is_some()).last() {
        let Some(st) = &evt.entered else {
            unreachable!()
        };
        let next = if let SingleplayerState::Base(next) = st {
            next.clone()
        } else {
            ClientState::Other("sp")
        };
        if *old_base != next {
            info!(?next, "setting client state");
            next_base.set(next);
        }
        base_evt.clear();
    }
}
