use bevy::prelude::*;
use factor_server::ServerPlugin;

fn main() {
    App::new()
        .add_plugins(MinimalPlugins)
        .add_plugins(ServerPlugin::default())
        .run();
}
