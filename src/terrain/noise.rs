use crate::healpix;
use bevy::math::Vec2;
use bevy::utils::HashMap;
use ordered_float::OrderedFloat;
use std::f64::consts::{FRAC_PI_2, TAU};
use std::sync::{Mutex, PoisonError};

/// Default interpolator function
pub fn interpolate(a0: f32, a1: f32, w: f32) -> f32 {
    (a1 - a0) * (3.0 - w * 2.0) * w * w + a0
}

/// A source of noise for a height map
pub trait NoiseSource {
    fn get_height(&self, lon: f32, lat: f32) -> f32;
}

/// A function that takes a longitude and latitude and returns a pseudo-random value
pub trait ValueHash {
    fn pos_hash(&self, lon: f32, lat: f32) -> f32;
}
impl<F: Fn(f32, f32) -> f32> ValueHash for F {
    fn pos_hash(&self, lon: f32, lat: f32) -> f32 {
        self(lon, lat)
    }
}

/// A function that takes a longitude and latitude and returns a pseudo-random vector
pub trait GradientHash {
    fn grad_hash(&self, lon: f32, lat: f32) -> Vec2;
}
impl<F: Fn(f32, f32) -> Vec2> GradientHash for F {
    fn grad_hash(&self, lon: f32, lat: f32) -> Vec2 {
        self(lon, lat)
    }
}

/// An oracle is a "hash" function that generates random values and stores them in a lookup table
#[derive(Debug)]
pub struct Oracle<F, V> {
    rand: F,
    lookup: Mutex<HashMap<[OrderedFloat<f32>; 2], V>>,
}
impl<F, V> Oracle<F, V> {
    pub fn new(rand: F) -> Self {
        Self {
            rand,
            lookup: Mutex::new(HashMap::new()),
        }
    }
}
impl<F: Fn() -> V, V: Copy> Oracle<F, V> {
    pub fn get(&self, lon: f32, lat: f32) -> V {
        let mut lock = self.lookup.lock().unwrap_or_else(PoisonError::into_inner);
        *lock
            .entry([
                OrderedFloat((lon * 100.0).round()),
                OrderedFloat((lat * 100.0).round()),
            ])
            .or_insert_with(&self.rand)
    }
}
impl<F: Fn() -> f32> ValueHash for Oracle<F, f32> {
    fn pos_hash(&self, lon: f32, lat: f32) -> f32 {
        self.get(lon, lat)
    }
}
impl<F: Fn() -> Vec2> GradientHash for Oracle<F, Vec2> {
    fn grad_hash(&self, lon: f32, lat: f32) -> Vec2 {
        self.get(lon, lat)
    }
}

/// A function to interpolate between two values
pub trait Interpolator {
    fn interp(&self, a0: f32, a1: f32, w: f32) -> f32;
}
impl<F: Fn(f32, f32, f32) -> f32> Interpolator for F {
    fn interp(&self, a0: f32, a1: f32, w: f32) -> f32 {
        self(a0, a1, w)
    }
}

/// Value noise
///
/// Takes a value hasher and an interpolator, along with the nested healpix depth
#[derive(Debug, Clone, Copy)]
pub struct ValueNoise<H, I> {
    pub hasher: H,
    pub interp: I,
    pub depth: u8,
}
impl<H, I> ValueNoise<H, I> {
    pub const fn new(depth: u8, hasher: H, interp: I) -> Self {
        Self {
            depth,
            hasher,
            interp,
        }
    }
}
impl<H: ValueHash, I: Interpolator> NoiseSource for ValueNoise<H, I> {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        let layer = healpix::nested::get(self.depth);
        let hash = layer.hash(
            (lon as f64) % TAU,
            (lat as f64).clamp(-FRAC_PI_2, FRAC_PI_2),
        );
        let vec = Vec2::new(lon, lat);
        let verts = layer.vertices(hash).map(|(x, y)| Vec2::new(x as _, y as _));
        let weights = verts.map(|v| self.hasher.pos_hash(v.x, v.y));
        let d1 = verts[0] - verts[1];
        let w1 = d1.dot(vec - verts[1]).max(0.0) / d1.length();
        let v1 = self.interp.interp(weights[0], weights[1], w1);
        let d2 = verts[2] - verts[3];
        let w2 = d2.dot(vec - verts[3]).max(0.0) / d2.length();
        let v2 = self.interp.interp(weights[2], weights[3], w2);
        let m1 = (verts[0] + verts[1]) * 0.5;
        let m2 = (verts[2] + verts[3]) * 0.5;
        let d3 = m1 - m2;
        let w3 = d3.dot(vec - m2).max(0.0) / d3.length();
        self.interp.interp(v1, v2, w3)
    }
}

/// Gradient (Perlin) noise
///
/// Takes a value hasher and an interpolator, along with the nested healpix depth
#[derive(Debug, Clone, Copy)]
pub struct GradientNoise<H, I> {
    pub hasher: H,
    pub interp: I,
    pub depth: u8,
}
impl<H, I> GradientNoise<H, I> {
    pub const fn new(depth: u8, hasher: H, interp: I) -> Self {
        Self {
            depth,
            hasher,
            interp,
        }
    }
}
impl<H: GradientHash, I: Interpolator> NoiseSource for GradientNoise<H, I> {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        let layer = healpix::nested::get(self.depth);
        let hash = layer.hash(
            (lon as f64) % TAU,
            (lat as f64).clamp(-FRAC_PI_2, FRAC_PI_2),
        );
        let vec = Vec2::new(lon, lat);
        let verts = layer
            .vertices(hash)
            .map(|(x, y)| Vec2::new(x as f32 % std::f32::consts::TAU, y as _));
        let weights = verts.map(|v| self.hasher.grad_hash(v.x, v.y).dot(vec - v));
        let v1 = self.interp.interp(
            weights[0],
            weights[1],
            (verts[0] - verts[1]).normalize().dot(vec - verts[0]),
        );
        let v2 = self.interp.interp(
            weights[2],
            weights[3],
            (verts[2] - verts[3]).normalize().dot(vec - verts[2]),
        );
        self.interp.interp(
            v1,
            v2,
            ((verts[0] + verts[1]) - (verts[2] + verts[3]))
                .normalize()
                .dot(vec - (verts[2] + verts[3]) * 0.5),
        )
    }
}
impl<T: NoiseSource, const N: usize> NoiseSource for [T; N] {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        self.iter().map(|n| n.get_height(lon, lat)).sum()
    }
}
impl<T: NoiseSource> NoiseSource for [T] {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        self.iter().map(|n| n.get_height(lon, lat)).sum()
    }
}
impl<T: NoiseSource> NoiseSource for &T {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        T::get_height(self, lon, lat)
    }
}
impl<T: NoiseSource> NoiseSource for (T, f32) {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        self.0.get_height(lon, lat) * self.1
    }
}
