use bevy::prelude::*;
use factor_client::{default_style, ClientPlugin};

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                fit_canvas_to_parent: true,
                title: "Factor- Client Only".into(),
                ..default()
            }),
            ..default()
        }))
        .add_plugins(ClientPlugin {
            can_use_singleplayer: false,
            egui_style: default_style(),
        })
        .run();
}
