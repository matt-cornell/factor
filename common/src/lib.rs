#![feature(slice_split_once)]

pub mod coords;
pub mod glue;
pub mod healpix;
pub mod noise;
pub mod rcu;
pub mod util;

pub const PLANET_RADIUS: f64 = 1_000_000.0;
