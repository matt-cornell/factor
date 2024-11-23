use bevy::prelude::*;
use factor::CombinedPlugin;
use factor_client::ClientPlugin;
use factor_server::ServerPlugin;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(ServerPlugin)
        .add_plugins(ClientPlugin {
            can_use_singleplayer: true,
        })
        .add_plugins(CombinedPlugin)
        .run();
}
