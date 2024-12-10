use bevy::prelude::*;
use factor::CombinedPlugin;
use factor_client::{default_style, ClientPlugin};
use factor_server::ServerPlugin;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(ServerPlugin::default())
        .add_plugins(ClientPlugin {
            can_use_singleplayer: true,
            egui_style: default_style(),
        })
        .add_plugins(CombinedPlugin)
        .run();
}
