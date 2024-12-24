#![feature(slice_split_once)]

pub mod cell;
pub mod coords;
pub mod data;
pub mod glue;
pub mod healpix;
pub mod mesh;
pub mod rcu;
pub mod util;

pub const PLANET_RADIUS: f64 = 6_000_000.0;
