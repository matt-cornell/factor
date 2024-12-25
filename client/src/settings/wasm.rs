use super::*;

pub fn load_config(mut commands: Commands) {
    warn!("Loading config isn't persistent on wasm");
    let mut inputs = InputMap::default();
    fill_keybinds(&mut inputs);
    commands.insert_resource(ClientSettings::default());
    commands.insert_resource(inputs);
    commands.insert_resource(TargetFps::Unlimited);
    commands.init_resource::<ActionState<Action>>();
}
pub fn save_config() {
    warn!("Saving config is a no-op on wasm");
}
