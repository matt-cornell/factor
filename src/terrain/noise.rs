use crate::healpix;
use bevy::math::Vec2;
use bevy::utils::HashMap;
use ordered_float::OrderedFloat;
use std::f64::consts::{FRAC_PI_2, TAU};
use std::sync::{Mutex, PoisonError};

fn circle_dist(a: Vec2, b: Vec2) -> Vec2 {
    use std::f32::consts::*;
    let mut dist = a - b;
    if dist.x > PI {
        dist.x -= TAU;
    }
    dist
}

/// Default interpolator function
pub fn interpolate(a0: f32, a1: f32, w: f32) -> f32 {
    (a1 - a0) * (3.0 - w * 2.0) * w * w + a0
}

/// A source of noise for a height map
pub trait NoiseSource {
    fn get_height(&self, lon: f32, lat: f32) -> f32;
}

#[derive(Debug, Clone, Copy)]
pub struct NoiseFn<F>(pub F);
impl<F: Fn(f32, f32) -> f32> NoiseSource for NoiseFn<F> {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        self.0(lon, lat)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Shifted<N> {
    pub base: N,
    pub shift: f32,
}
impl<N> Shifted<N> {
    pub const fn new(base: N, shift: f32) -> Self {
        Self { base, shift }
    }
}
impl<N: NoiseSource> NoiseSource for Shifted<N> {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        self.base
            .get_height((lon + self.shift) % std::f32::consts::TAU, lat)
    }
}

/// A function that takes a longitude and latitude and returns a pseudo-random value
pub trait ValueHash<I, O> {
    fn hash(&self, input: I) -> O;
}
impl<O, F: Fn(f32, f32) -> O> ValueHash<(f32, f32), O> for F {
    fn hash(&self, (lon, lat): (f32, f32)) -> O {
        self(lon, lat)
    }
}

/// An oracle is a "hash" function that generates random values and stores them in a lookup table
#[derive(Debug)]
pub struct PosOracle<F, V> {
    rand: F,
    lookup: Mutex<HashMap<[OrderedFloat<f32>; 2], V>>,
}
impl<F, V> PosOracle<F, V> {
    pub fn new(rand: F) -> Self {
        Self {
            rand,
            lookup: Mutex::new(HashMap::new()),
        }
    }
}
impl<F: Fn() -> V, V: Copy> PosOracle<F, V> {
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
impl<F: Fn() -> O, O: Copy> ValueHash<(f32, f32), O> for PosOracle<F, O> {
    fn hash(&self, (lon, lat): (f32, f32)) -> O {
        self.get(lon, lat)
    }
}

impl<O: Copy> ValueHash<usize, O> for Box<[O]> {
    fn hash(&self, input: usize) -> O {
        self[input]
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

pub trait Scaler {
    fn scale(&self, w: f32) -> f32;
}
impl<F: Fn(f32) -> f32> Scaler for F {
    fn scale(&self, w: f32) -> f32 {
        self(w)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Identity;
impl Scaler for Identity {
    fn scale(&self, w: f32) -> f32 {
        w
    }
}

/// Value noise
///
/// Takes a value hasher and an interpolator, along with the nested healpix depth
#[derive(Debug, Clone, Copy)]
pub struct ValueCornerNoise<H, I> {
    pub depth: u8,
    pub hasher: H,
    pub interp: I,
}
impl<H, I> ValueCornerNoise<H, I> {
    pub const fn new(depth: u8, hasher: H, interp: I) -> Self {
        Self {
            depth,
            hasher,
            interp,
        }
    }
}
impl<H: ValueHash<(f32, f32), f32>, I: Interpolator> NoiseSource for ValueCornerNoise<H, I> {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        let layer = healpix::nested::get(self.depth);
        let hash = layer.hash(
            (lon as f64) % TAU,
            (lat as f64).clamp(-FRAC_PI_2, FRAC_PI_2),
        );
        let vec = Vec2::new(lon, lat);
        let verts = layer.vertices(hash).map(|(x, y)| Vec2::new(x as _, y as _));
        let weights = verts.map(|v| self.hasher.hash((v.x, v.y)));
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
pub struct GradientCornerNoise<H, I> {
    pub depth: u8,
    pub hasher: H,
    pub interp: I,
}
impl<H, I> GradientCornerNoise<H, I> {
    pub const fn new(depth: u8, hasher: H, interp: I) -> Self {
        Self {
            depth,
            hasher,
            interp,
        }
    }
}
impl<H: ValueHash<(f32, f32), Vec2>, I: Interpolator> NoiseSource for GradientCornerNoise<H, I> {
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
        let weights = verts.map(|v| self.hasher.hash((v.x, v.y)).dot(vec - v));
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

#[derive(Debug, Clone, Copy)]
pub struct ValueCellNoise<H, S> {
    pub depth: u8,
    pub hasher: H,
    pub scale: S,
}
impl<H> ValueCellNoise<H, Identity> {
    pub const fn new(depth: u8, hasher: H) -> Self {
        Self {
            depth,
            hasher,
            scale: Identity,
        }
    }
}
impl<H, S> ValueCellNoise<H, S> {
    pub const fn with_scale(depth: u8, hasher: H, scale: S) -> Self {
        Self {
            depth,
            hasher,
            scale,
        }
    }
}
impl<H: ValueHash<usize, f32>, S: Scaler> NoiseSource for ValueCellNoise<H, S> {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        let layer = healpix::nested::get(self.depth);
        let neighs = layer.bilinear_interpolation(
            (lon as f64) % TAU,
            (lat as f64).clamp(-FRAC_PI_2, FRAC_PI_2),
        );
        let Vec2 { x: sum, y: scale } = neighs
            .iter()
            .map(|&(n, w)| {
                let s = self.scale.scale(w as f32);
                Vec2::new(self.hasher.hash(n as _) * s, s)
            })
            .sum();
        sum / scale
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GradientCellNoise<H, S> {
    pub depth: u8,
    pub hasher: H,
    pub scale: S,
}
impl<H> GradientCellNoise<H, Identity> {
    pub const fn new(depth: u8, hasher: H) -> Self {
        Self {
            depth,
            hasher,
            scale: Identity,
        }
    }
}
impl<H, S> GradientCellNoise<H, S> {
    pub const fn with_scale(depth: u8, hasher: H, scale: S) -> Self {
        Self {
            depth,
            hasher,
            scale,
        }
    }
}
impl<H: ValueHash<usize, Vec2>, S: Scaler> NoiseSource for GradientCellNoise<H, S> {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        let layer = healpix::nested::get(self.depth);
        let neighs = layer.bilinear_interpolation(
            (lon as f64) % TAU,
            (lat as f64).clamp(-FRAC_PI_2, FRAC_PI_2),
        );
        let vec = Vec2::new(lon, lat);
        let Vec2 { x: sum, y: scale } = neighs
            .iter()
            .map(|&(n, w)| {
                let (cx, cy) = layer.center(n as _);
                let c = Vec2::new(cx as _, cy as _);
                let s = self.scale.scale(w as f32);
                Vec2::new(
                    self.hasher
                        .hash(n as _)
                        .dot(circle_dist(vec, c).normalize_or_zero())
                        .mul_add(0.5, 0.5)
                        * s,
                    s,
                )
            })
            .sum();
        sum / scale
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
impl<T: NoiseSource> NoiseSource for Box<T> {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        T::get_height(self, lon, lat)
    }
}
impl<T: NoiseSource> NoiseSource for std::sync::Arc<T> {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        T::get_height(self, lon, lat)
    }
}
impl<T: NoiseSource> NoiseSource for (T, f32) {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        self.0.get_height(lon, lat) * self.1
    }
}
impl<T: NoiseSource> NoiseSource for (T, [f32; 2]) {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        self.0.get_height(lon, lat).mul_add(self.1[0], self.1[1])
    }
}
macro_rules! impl_for_tuple {
    () => {};
    ($first:ident $(, $rest:ident)*) => {
        impl<$first: NoiseSource, $($rest: NoiseSource,)*> NoiseSource for ($first, $($rest,)*) {
            #[allow(non_snake_case, unused_mut)]
            fn get_height(&self, lon: f32, lat: f32) -> f32 {
                let ($first, $($rest,)*) = self;
                let mut out = $first.get_height(lon, lat);
                $(out += $rest.get_height(lon, lat);)*
                out
            }
        }
        impl_for_tuple!($($rest),*);
    };
}
impl_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);
