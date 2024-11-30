use bevy::math::DVec2;
use std::f64::consts::{FRAC_PI_2, TAU};

/// A longitude-latitude pair of coordinates. Normalizes and handles conversions between `f32` and `f64`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LonLat {
    pub lon: f64,
    pub lat: f64,
}
impl LonLat {
    pub const fn from_f64(lon: f64, lat: f64) -> Self {
        Self { lon, lat }
    }
    pub const fn from_f32(lon: f32, lat: f32) -> Self {
        Self {
            lon: lon as _,
            lat: lat as _,
        }
    }
    pub fn normalized(self) -> Self {
        Self {
            lon: self.lon % TAU,
            lat: self.lat.clamp(-FRAC_PI_2, FRAC_PI_2),
        }
    }
    pub fn normalize(&mut self) {
        self.lon %= TAU;
        self.lat = self.lat.clamp(-FRAC_PI_2, FRAC_PI_2);
    }
    pub fn as_f64(self) -> (f64, f64) {
        (self.lon % TAU, self.lat.clamp(-FRAC_PI_2, FRAC_PI_2))
    }
    pub fn as_f32(self) -> (f32, f32) {
        use std::f32::consts::{FRAC_PI_2, TAU};
        (
            self.lon as f32 % TAU,
            (self.lat as f32).clamp(-FRAC_PI_2, FRAC_PI_2),
        )
    }
}
impl From<(f32, f32)> for LonLat {
    fn from(value: (f32, f32)) -> Self {
        Self {
            lon: value.0 as _,
            lat: value.1 as _,
        }
    }
}
impl From<(f64, f64)> for LonLat {
    fn from(value: (f64, f64)) -> Self {
        Self {
            lon: value.0,
            lat: value.1,
        }
    }
}
impl From<LonLat> for (f32, f32) {
    fn from(value: LonLat) -> Self {
        value.as_f32()
    }
}
impl From<LonLat> for (f64, f64) {
    fn from(value: LonLat) -> Self {
        value.as_f64()
    }
}

/// Get the offset of a point from a reference starting point.
pub fn get_relative(start: LonLat, end: LonLat) -> DVec2 {
    let LonLat { lon: lo1, lat: la1 } = start;
    let LonLat { lon: lo2, lat: la2 } = end;
    let dlo = lo2 - lo1;
    let dla = la2 - la1;
    let (dlosin, dlocos) = dlo.sin_cos();
    let (la1sin, la1cos) = la1.sin_cos();
    let (la2sin, la2cos) = la2.sin_cos();
    let azimuth = (dlosin * la2cos).atan2(la1cos * la2sin - la1sin * la2cos * dlocos);
    let a = (dla * 0.5).sin().powi(2) + la1cos * la2cos * (dlo * 0.5).sin().powi(2);
    let dist = a.sqrt().atan2((1.0 - a).sqrt());
    DVec2::from_angle(azimuth) * dist * 2.0
}

/// Get the ending point on a sphere given an offset and starting point.
pub fn get_absolute(start: LonLat, offset: DVec2) -> LonLat {
    let LonLat { lon: lo1, lat: la1 } = start;
    let azimuth = offset.to_angle();
    let dist = offset.length();
    let (lasin, lacos) = la1.sin_cos();
    let (azsin, azcos) = azimuth.sin_cos();
    let (dsin, dcos) = dist.sin_cos();
    let la2 = (lasin * dcos + lacos * dsin * azcos).asin();
    let lo2 = lo1 + (azsin * dsin * lacos).atan2(dcos - lasin * la2.sin());
    LonLat::from_f64(lo2, la2)
}
