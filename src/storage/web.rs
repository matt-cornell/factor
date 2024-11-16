use base64::alphabet::Alphabet;
use base64::engine::{DecodePaddingMode, GeneralPurpose, GeneralPurposeConfig};
use base64::Engine;
use redb::{DatabaseError, StorageBackend};
use std::io;
use wasm_bindgen::UnwrapThrowExt;
use web_sys::Storage;

const BASE64_CODEC: GeneralPurpose = GeneralPurpose::new(
    &match Alphabet::new("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/") {
        Ok(a) => a,
        Err(_) => panic!("invalid alphabet!"),
    },
    GeneralPurposeConfig::new()
        .with_encode_padding(false)
        .with_decode_allow_trailing_bits(true)
        .with_decode_padding_mode(DecodePaddingMode::Indifferent),
);

pub fn local_storage() -> Storage {
    web_sys::window()
        .expect_throw("No window to load for storage!")
        .local_storage()
        .expect_throw("Failed to get localStorage")
        .expect_throw("localStorage doesn't exist!")
}

const BLOCK_BITS: u32 = 20;
const BLOCK_SIZE: usize = 1 << BLOCK_BITS;
const BLOCK_MASK: usize = (1 << BLOCK_BITS) - 1;

/// A persistent `redb` backend that can be saved and loaded by a name.
/// On native platforms, it saves it to a file called `<name>.redb`
/// On the web (this implementation), it save data in local storage, with keys prefixed by `factorsave-<name>`
#[derive(Debug)]
pub struct PersistentBackend {
    name: String,
}
impl PersistentBackend {
    /// Create a new database.
    /// Fails on native if creating the home directory can't be found, creating/opening the file fails, or the database is already open.
    /// Never fails in the web.
    pub fn new(name: String) -> Result<Self, DatabaseError> {
        Ok(Self { name })
    }
    /// Delete a save with the given name.
    /// Fails if the associated file operation fails on native platforms.
    /// Never fails on web.
    pub fn delete_save(name: &str) -> io::Result<()> {
        let storage = local_storage();
        let length = storage.length().unwrap_throw();
        let prefix = format!("factorsave-{name}");
        let prune = (0..length)
            .filter_map(|i| {
                let name = storage.key(i).ok()??;
                name.starts_with(&prefix).then_some(name)
            })
            .collect::<Vec<_>>();
        for key in prune {
            let _ = storage.remove_item(&key);
        }
        Ok(())
    }
    /// List the loadable saves. Returns an empty iterator if we can't open a necessary file, skips files that can't be opened.
    pub fn list_saves() -> impl Iterator<Item = String> {
        let storage = local_storage();
        let length = storage.length().unwrap_throw();
        (0..length).filter_map(move |i| {
            let mut name = storage.key(i).ok()??;
            if name.ends_with("-len") {
                name.truncate(name.len() - 4);
            }
            if name.starts_with("factorsave-") {
                name.replace_range(0..11, "");
                Some(name)
            } else {
                None
            }
        })
    }
    fn load_block(&self, idx: usize, storage: &Storage) -> io::Result<Option<String>> {
        let key = format!("factorsave-{}-{idx:>010x}", self.name);
        storage.get_item(&key).map_err(|err| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("Error reading localStorage key '{key}': {err:?}"),
            )
        })
    }
    fn save_block(&self, idx: usize, storage: &Storage, data: &str) -> io::Result<()> {
        let key = format!("factorsave-{}-{idx:>010x}", self.name);
        storage.set_item(&key, data).map_err(|err| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("Error reading localStorage key '{key}': {err:?}"),
            )
        })
    }
}
impl StorageBackend for PersistentBackend {
    fn len(&self) -> io::Result<u64> {
        let storage = local_storage();
        let key = format!("factorsave-{}-len", self.name);
        let len = storage.get_item(&key).map_err(|err| {
            io::Error::new(
                io::ErrorKind::Other,
                format!(
                    "Error reading localStorage key 'factorsave-{}-len': {err:?}",
                    self.name
                ),
            )
        })?;
        match len {
            Some(s) => s
                .parse()
                .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err)),
            None => {
                storage.set_item(&key, "0").map_err(|err| {
                    io::Error::new(
                        io::ErrorKind::Other,
                        format!(
                            "Error setting localStorage key 'factorsave-{}-len': {err:?}",
                            self.name
                        ),
                    )
                })?;
                Ok(0)
            }
        }
    }
    fn read(&self, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        let offset = offset as usize;
        let end = offset + len;
        let start_block = offset >> BLOCK_BITS;
        let end_block = end >> BLOCK_BITS;
        let start_off = offset & BLOCK_MASK;
        let end_off = end & BLOCK_MASK;
        let mut out = Vec::with_capacity(len);
        let storage = local_storage();
        if start_block == end_block {
            if let Some(data) = self.load_block(start_block, &storage)? {
                let start = start_off / 3 * 4; // do it like this to floor to the start of the block
                let end = (end_off + 2) / 3 * 4; // and this gets the ceiling of the next block
                let real_start = start * 3 / 4;
                let real_end = end * 3 / 4;
                BASE64_CODEC
                    .decode_vec(&data[start..end], &mut out)
                    .unwrap();
                out.drain(..(start_off - real_start));
                out.truncate(out.len() - real_end - end_off);
            } else {
                out.resize(len, 0);
            }
        } else {
            {
                if let Some(data) = self.load_block(start_block, &storage)? {
                    let start = start_off / 3 * 4; // do it like this to floor to the start of the block
                    let real_start = start * 3 / 4;
                    BASE64_CODEC.decode_vec(&data[start..], &mut out).unwrap();
                    out.drain(..(start_off - real_start));
                } else {
                    out.resize(BLOCK_SIZE - start_off, 0);
                }
            }
            for blk in (start_block + 1)..end_block {
                if let Some(data) = self.load_block(blk, &storage)? {
                    BASE64_CODEC.decode_vec(&data, &mut out).unwrap();
                } else {
                    out.resize(out.len() + BLOCK_SIZE, 0);
                }
            }
            {
                if let Some(data) = self.load_block(end_block, &storage)? {
                    let end = (end_off + 2) / 3 * 4; // and this gets the ceiling of the next block
                    let real_end = end * 3 / 4;
                    BASE64_CODEC.decode_vec(&data[..end], &mut out).unwrap();
                    out.truncate(out.len() - (real_end - end_off));
                } else {
                    out.resize(out.len() + end_off, 0);
                }
            }
        }
        Ok(out)
    }
    fn set_len(&self, len: u64) -> io::Result<()> {
        let storage = local_storage();
        let key = format!("factorsave-{}-len", self.name);
        storage.set_item(&key, &len.to_string()).map_err(|err| {
            io::Error::new(
                io::ErrorKind::Other,
                format!(
                    "Error setting localStorage key 'factorsave-{}-len': {err:?}",
                    self.name
                ),
            )
        })?;
        Ok(())
    }
    #[inline]
    fn sync_data(&self, _eventual: bool) -> io::Result<()> {
        Ok(())
    }
    fn write(&self, offset: u64, mut data: &[u8]) -> io::Result<()> {
        let offset = offset as usize;
        let end = offset + data.len();
        let start_block = offset >> BLOCK_BITS;
        let end_block = end >> BLOCK_BITS;
        let start_off = offset & BLOCK_MASK;
        let end_off = end & BLOCK_MASK;
        let mut block = [0u8; BLOCK_SIZE];
        let mut encoded = String::new();
        let storage = local_storage();
        if start_block == end_block {
            if let Some(data) = self.load_block(start_block, &storage)? {
                BASE64_CODEC.decode_slice(&data, &mut block).unwrap();
            }
            block[start_off..end_off].copy_from_slice(data);
            BASE64_CODEC.encode_string(block, &mut encoded);
            self.save_block(start_block, &storage, &encoded)?;
        } else {
            {
                if let Some(data) = self.load_block(start_block, &storage)? {
                    BASE64_CODEC.decode_slice(&data, &mut block).unwrap();
                }
                let len = BLOCK_SIZE - start_off;
                let (head, tail) = data.split_at(len);
                data = tail;
                block[start_off..].copy_from_slice(head);
                BASE64_CODEC.encode_string(block, &mut encoded);
                self.save_block(start_block, &storage, &encoded)?;
            }
            for blk in (start_block + 1)..end_block {
                encoded.clear();
                let (head, tail) = data.split_at(BLOCK_SIZE);
                data = tail;
                BASE64_CODEC.encode_string(head, &mut encoded);
                self.save_block(blk, &storage, &encoded)?;
            }
            {
                encoded.clear();
                if let Some(data) = self.load_block(start_block, &storage)? {
                    BASE64_CODEC.decode_slice(&data, &mut block).unwrap();
                } else {
                    block[..end_off].fill(0);
                }
                block[..end_off].copy_from_slice(data);
                BASE64_CODEC.encode_string(block, &mut encoded);
                self.save_block(start_block, &storage, &encoded)?;
            }
        }
        Ok(())
    }
}
