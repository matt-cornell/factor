use bevy::math::DVec2;
use healpix::coords::{LonLat, LonLatT};
pub use healpix::geo::distance;

#[inline(always)]
pub fn absolute(start: impl LonLatT, offset: DVec2) -> LonLat {
    healpix::geo::absolute(start, offset.into())
}

#[inline(always)]
pub fn relative(start: impl LonLatT, end: impl LonLatT) -> DVec2 {
    healpix::geo::relative(start, end).into()
}
