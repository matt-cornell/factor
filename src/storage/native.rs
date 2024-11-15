use bevy::log::*;
use etcetera::*;
use redb::{backends::FileBackend, *};
use std::path::PathBuf;
use std::{fs, io};

/// A persistent `redb` backend that can be saved and loaded by a name.
/// On native platforms (this implementation), it saves it to a file called `<name>.redb`
/// On the web, it save data in local storage, with keys prefixed by `factorsave-<name>`
#[derive(Debug)]
pub struct PersistentBackend {
    inner: FileBackend,
}
impl PersistentBackend {
    /// The data directory to use. This doesn't have a web counterpart.
    pub fn data_dir() -> Result<PathBuf, HomeDirError> {
        match choose_app_strategy(AppStrategyArgs {
            top_level_domain: "com".to_string(),
            author: "tbd".to_string(),
            app_name: "Factor".to_string(),
        }) {
            Ok(strat) => Ok(strat.data_dir()),
            Err(err) => {
                error!(%err, "Couldn't locate home directory");
                Err(err)
            }
        }
    }
    /// Create a new database.
    /// Fails on native if creating the home directory can't be found, creating/opening the file fails, or the database is already open.
    /// Never fails in the web.
    pub fn new(name: String) -> Result<Self, DatabaseError> {
        info_span!("Creating persistent backend", name);
        let mut saves = Self::data_dir().map_err(|_| {
            error!("Couldn't find a home directory");
            io::Error::new(io::ErrorKind::Other, "Couldn't find a home directory")
        })?;
        saves.push("saves");
        info!(dir = %saves.display(), "Found save directory");
        if !saves.exists() {
            debug!(dir = %saves.display(), "Creating save directory");
            if let Err(err) = fs::create_dir_all(&saves) {
                error!(%err, "Error creating save directory");
                Err(err)?;
            }
        }
        saves.push(name);
        saves.add_extension("redb");
        let res = fs::OpenOptions::new()
            .read(true)
            .append(true)
            .create(true)
            .open(saves);
        match res {
            Ok(file) => match FileBackend::new(file) {
                Ok(inner) => Ok(Self { inner }),
                Err(err) => {
                    error!(%err, "Failed to open database");
                    Err(err)
                }
            },
            Err(err) => {
                error!(%err, "Failed to open file");
                Err(err)?
            }
        }
    }
    /// Delete a save with the given name.
    /// Fails if the associated file operation fails on native platforms.
    /// Never fails on web.
    pub fn delete_save(name: &str) -> io::Result<()> {
        let mut saves = Self::data_dir().map_err(|_| {
            error!("Couldn't find a home directory");
            io::Error::new(io::ErrorKind::Other, "Couldn't find a home directory")
        })?;
        saves.push("saves");
        saves.push(name);
        saves.add_extension("redb");
        info!(path = %saves.display(), "Deleting save file");
        if let Err(err) = fs::remove_file(&saves) {
            error!(%err, "Failed to delete file");
            Err(err)
        } else {
            Ok(())
        }
    }
    /// List the loadable saves. Returns an empty iterator if we can't open a necessary file, skips files that can't be opened.
    pub fn list_saves() -> impl Iterator<Item = String> {
        info_span!("Loading save names");
        let Ok(mut saves) = Self::data_dir() else {
            error!("Couldn't find home directory");
            return None.into_iter().flatten();
        };
        saves.push("saves");
        if !saves.exists() {
            debug!("Save directory doesn't exist");
            None.into_iter().flatten()
        } else {
            debug!(dir = %saves.display(), "Reading save directory");
            match fs::read_dir(saves) {
                Ok(saves) => Some(saves.filter_map(|e| match e {
                    Ok(entry) => match entry.file_name().into_string() {
                        Ok(mut name) => {
                            let path = entry.path();
                            if let Some(n) = name.strip_suffix(".redb") {
                                name.truncate(n.len());
                                let mut opts = fs::OpenOptions::new();
                                #[cfg(unix)]
                                // we're just checking to see if we can open the file, so we want to avoid setting the atime
                                std::os::unix::fs::OpenOptionsExt::custom_flags(
                                    &mut opts,
                                    libc::O_NOATIME,
                                );
                                if opts.read(true).open(&path).is_ok() {
                                    Some(name)
                                } else {
                                    warn!(
                                        file = %path.display(),
                                        "Save file is not openable, skipping"
                                    );
                                    None
                                }
                            } else {
                                warn!(
                                    file = %path.display(),
                                    "Non-save file found in save directory, skipping"
                                );
                                None
                            }
                        }
                        Err(name) => {
                            warn!(?name, "Non-UTF8 file name, skipping");
                            None
                        }
                    },
                    Err(err) => {
                        warn!(%err, "Error reading entry");
                        None
                    }
                }))
                .into_iter()
                .flatten(),
                Err(err) => {
                    error!(%err, "Failed to read save directory");
                    None.into_iter().flatten()
                }
            }
        }
    }
}
impl StorageBackend for PersistentBackend {
    #[inline]
    fn len(&self) -> io::Result<u64> {
        self.inner.len()
    }
    #[inline]
    fn read(&self, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        self.inner.read(offset, len)
    }
    #[inline]
    fn set_len(&self, len: u64) -> io::Result<()> {
        self.inner.set_len(len)
    }
    #[inline]
    fn sync_data(&self, eventual: bool) -> io::Result<()> {
        self.inner.sync_data(eventual)
    }
    #[inline]
    fn write(&self, offset: u64, data: &[u8]) -> io::Result<()> {
        self.inner.write(offset, data)
    }
}
