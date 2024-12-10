use super::*;
use bevy::log::*;
use etcetera::*;
use std::io;
use std::path::PathBuf;

impl ClientSettings {
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
    pub fn load() -> Self {
        let mut this = None;
        let _res: io::Result<()> = try {
            let path = Self::path()?;
            std::fs::create_dir_all(path.parent().unwrap())?;
            let mut needs_write = true;
            let _: io::Result<()> = try {
                let data = std::fs::read_to_string(&path)
                    .inspect_err(|err| warn!(%err, "Failed to load settings from file"))?;
                let partial = toml::from_str::<PartialClientSettings>(&data).map_err(|err| {
                    error!(%err, "Invalid data in settings file");
                    io::Error::new(io::ErrorKind::InvalidData, "")
                })?;
                needs_write = partial.render_distance.is_none();
                this = Some(partial.into());
            };
            let cfg = this.get_or_insert_default();
            if needs_write {
                info!(?path, "Saving settings");
                std::fs::write(
                    path,
                    toml::to_string_pretty(&cfg)
                        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, ""))?,
                )
                .inspect_err(|err| error!(%err, "Failed to save settings"))?;
            }
        };
        this.unwrap_or_default()
    }
    pub fn save(&self) {
        let _res: io::Result<()> = try {
            let path = Self::path()?;
            std::fs::write(
                path,
                toml::to_string_pretty(self)
                    .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, ""))?,
            )
            .inspect_err(|err| error!(%err, "Failed to save settings"))?;
        };
    }
}
