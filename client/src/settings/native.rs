use super::*;
use etcetera::*;
use std::io;
use std::path::PathBuf;

fn path() -> io::Result<PathBuf> {
    match choose_app_strategy(AppStrategyArgs {
        top_level_domain: "com".to_string(),
        author: "tbd".to_string(),
        app_name: "Factor".to_string(),
    }) {
        Ok(strat) => {
            let mut path = strat.data_dir();
            path.push("settings.toml");
            Ok(path)
        }
        Err(err) => {
            error!(%err, "Couldn't locate home directory");
            Err(io::Error::new(
                io::ErrorKind::Other,
                "Couldn't locate home directory",
            ))
        }
    }
}

pub fn load_config(mut commands: Commands) {
    let mut res = None;
    let _res: io::Result<()> = try {
        let path = path()?;
        std::fs::create_dir_all(path.parent().unwrap())?;
        let mut needs_write = true;
        let _: io::Result<()> = try {
            let data = std::fs::read_to_string(&path)
                .inspect_err(|err| warn!(%err, "Failed to load settings from file"))?;
            let partial = toml::from_str::<DeserializeShim>(&data).map_err(|err| {
                error!(%err, "Invalid data in settings file");
                io::Error::new(io::ErrorKind::InvalidData, "")
            });
            let partial = match partial {
                Ok(part) => part,
                Err(err) => {
                    if let Ok(settings) = toml::from_str::<PartialClientSettings>(&data) {
                        DeserializeShim {
                            settings: Some(settings),
                            input: None,
                        }
                    } else {
                        Err(err)?
                    }
                }
            };
            needs_write = partial
                .settings
                .as_ref()
                .is_none_or(PartialClientSettings::is_incomplete)
                || partial.input.is_none();
            res = Some((
                partial
                    .settings
                    .map_or_else(ClientSettings::default, ClientSettings::from),
                partial.input.unwrap_or_default(),
            ));
        };
        let (settings, input) = res.get_or_insert_default();
        needs_write |= fill_keybinds(input);
        if needs_write {
            info!(?path, "Saving settings");
            std::fs::write(
                path,
                toml::to_string_pretty(&SerializeShim { settings, input })
                    .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, ""))?,
            )
            .inspect_err(|err| error!(%err, "Failed to save settings"))?;
        }
    };
    let (settings, input) = res.unwrap_or_else(|| {
        (ClientSettings::default(), {
            let mut map = InputMap::default();
            fill_keybinds(&mut map);
            map
        })
    });
    commands.insert_resource(TargetFps::from(settings.target_fps));
    commands.insert_resource(settings);
    commands.insert_resource(input);
    commands.init_resource::<ActionState<Action>>();
}

pub fn save_config(settings: Res<ClientSettings>, input: Res<InputMap<Action>>) {
    let _res: io::Result<()> = try {
        let path = path()?;
        std::fs::write(
            path,
            toml::to_string_pretty(&SerializeShim {
                settings: &settings,
                input: &input,
            })
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, ""))?,
        )
        .inspect_err(|err| error!(%err, "Failed to save settings"))?;
    };
}
