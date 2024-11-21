pub mod option_bytes {
    //! Optional bytes
    //!
    //! Taken from `hex::serde`, but adapted to use `Option<T>`.
    use hex::{FromHex, ToHex};
    use serde::de::{Deserializer, Error, Visitor};
    use serde::ser::Serializer;
    use std::fmt;
    use std::marker::PhantomData;

    pub fn serialize<S, T>(data: &Option<T>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        T: ToHex,
    {
        if let Some(data) = data {
            let s = data.encode_hex::<String>();
            serializer.serialize_str(&s)
        } else {
            serializer.serialize_none()
        }
    }
    pub fn deserialize<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>
    where
        D: Deserializer<'de>,
        T: FromHex,
        <T as FromHex>::Error: fmt::Display,
    {
        struct HexStrVisitor<T>(PhantomData<T>);

        impl<'de, T> Visitor<'de> for HexStrVisitor<T>
        where
            T: FromHex,
            <T as FromHex>::Error: fmt::Display,
        {
            type Value = Option<T>;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "a hex encoded string")
            }

            fn visit_str<E>(self, data: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                match FromHex::from_hex(data) {
                    Ok(v) => Ok(Some(v)),
                    Err(e) => Err(Error::custom(e)),
                }
            }

            fn visit_borrowed_str<E>(self, data: &'de str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                match FromHex::from_hex(data) {
                    Ok(v) => Ok(Some(v)),
                    Err(e) => Err(Error::custom(e)),
                }
            }
        }

        deserializer.deserialize_str(HexStrVisitor(PhantomData))
    }
}
