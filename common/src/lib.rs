#![feature(slice_split_once)]

pub mod cell;
pub mod coords;
pub mod data;
pub mod glue;
pub mod healpix;
pub mod mesh;
pub mod rcu;
pub mod util;

// Yes this is a bit bigger than the earth, but it makes the distnaces work out better.
pub const PLANET_RADIUS: f64 = 10_000_000.0;
