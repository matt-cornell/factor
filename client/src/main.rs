use bevy::prelude::*;
use factor_client::{default_style, ClientPlugin};

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(ClientPlugin {
            can_use_singleplayer: false,
            egui_style: default_style(),
        })
        .run();
}
