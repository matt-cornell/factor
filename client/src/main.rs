use bevy::prelude::*;
use factor_client::ClientPlugin;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(ClientPlugin {
            can_use_singleplayer: true,
        })
        .run();
}
