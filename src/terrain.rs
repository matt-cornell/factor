use bevy::math::Vec2;
use bytemuck::Zeroable;
use cdshealpix::compass_point::MainWind;
use cdshealpix::nested::bmoc::BMOC;
use itertools::Itertools;
use rand::distributions::Uniform;
use rand::prelude::*;
use std::f64::consts::FRAC_PI_8;
use std::fmt::{self, Debug, Formatter};

/// Either an `i32`, or an `f32`, for scratch purposes. Stored as a union because we know which one we need.
#[derive(Clone, Copy, Zeroable)]
pub union IntOrFloat {
    pub int: u32,
    pub float: f32,
}
impl IntOrFloat {
    pub const ZERO: Self = Self { int: 0 };
    pub const fn int(self) -> u32 {
        unsafe { self.int }
    }
    pub const fn float(self) -> f32 {
        unsafe { self.float }
    }
}
impl Default for IntOrFloat {
    fn default() -> Self {
        Self::ZERO
    }
}
impl Debug for IntOrFloat {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("IntOrFloat")
    }
}

/// A single cell for the plate tectonics
#[derive(Debug, Default, Clone, Copy, Zeroable)]
pub struct TectonicCell {
    pub age: u32,
    pub density: u32,
    pub dx: f32,
    pub dy: f32,
    pub height: f32,
    pub scratch: IntOrFloat,
}
impl TectonicCell {
    pub const fn motion(&self) -> Vec2 {
        Vec2::new(self.dx, self.dy)
    }
    pub fn add_motion(&mut self, delta: Vec2) {
        self.dx += delta.x;
        self.dy += delta.y;
    }
    pub fn add_lazy_motion(&mut self, delta: Vec2) {
        let d = self.scratch.int();
        let new_dx =
            (((d & 0xffff) as u16 as i16).saturating_add((delta.x * 8192.0) as i16)) as u16 as u32;
        let new_dy = ((((d >> 16) & 0xffff) as u16 as i16).saturating_add((delta.y * 8192.0) as i16)
            as u16 as u32)
            << 16;
        self.scratch.int = new_dx | new_dy;
    }
    pub fn resolve_lazy_motion(&mut self) {
        let d = self.scratch.int();
        self.dx += (d & 0xffff) as u16 as i16 as f32 / 8192.0;
        self.dy += ((d >> 16) & 0xffff) as u16 as i16 as f32 / 8192.0;
        self.scratch.int = 0;
    }
}

pub fn init_terrain<R: Rng + ?Sized>(depth: u8, rng: &mut R) -> Box<[TectonicCell]> {
    let layer = cdshealpix::nested::get(depth);
    let len = layer.n_hash() as _;
    let age_range = Uniform::new(100000, 1000000);
    let density_range = Uniform::new(1000, 10000);
    let mut terr = Box::new_uninit_slice(len);
    for cell in &mut terr {
        let density = rng.sample(age_range);
        cell.write(TectonicCell {
            age: 0,
            density,
            dx: 0.0,
            dy: 0.0,
            height: 0.0,
            scratch: IntOrFloat::ZERO,
        });
    }
    let mut terr = unsafe { terr.assume_init() };
    let mut neighbors = Vec::new();
    for i in 0..len {
        neighbors.clear();
        layer.append_bulk_neighbours(i as _, &mut neighbors);
        let age = neighbors
            .iter()
            .map(|&i| terr[i as usize].density)
            .sum::<u32>()
            / neighbors.len() as u32;
        let cell = &mut terr[i];
        cell.age = cell.density / 10 + age;
        cell.scratch.int = rng.sample(density_range) + cell.age / 20;
    }
    for i in 0..len {
        neighbors.clear();
        layer.append_bulk_neighbours(i as _, &mut neighbors);
        let density = neighbors
            .iter()
            .map(|&i| terr[i as usize].scratch.int())
            .sum::<u32>()
            / neighbors.len() as u32;
        let cell = &mut terr[i];
        cell.density = cell.scratch.int() / 10 + density;
    }
    for cell in &mut terr {
        cell.scratch.int = 0x00000000;
    }
    terr
}
pub fn step_terrain<R: Rng + ?Sized>(depth: u8, terrain: &mut [TectonicCell], rng: &mut R) {
    const WIND_VECS: [Vec2; 9] = [
        Vec2::new(0.0, -1.0),
        Vec2::new(0.7, -0.7),
        Vec2::new(1.0, 0.0),
        Vec2::new(-0.7, -0.7),
        Vec2::ZERO,
        Vec2::new(0.7, 0.7),
        Vec2::new(-1.0, 0.0),
        Vec2::new(0.7, -0.7),
        Vec2::new(0.0, 1.0),
    ];
    let layer = cdshealpix::nested::get(depth);
    debug_assert_eq!(layer.n_hash(), terrain.len() as u64);
    let currents = Uniform::new(-0.05, 0.05);
    let density_range = Uniform::new(1000, 10000);
    for i in 0..terrain.len() {
        let cell = &mut terrain[i];
        let d = cell.density as f32 / cell.age as f32;
        cell.age += 2000;
        cell.density = (cell.age as f32 * d) as _;
        cell.dx += rng.sample(currents);
        cell.dy += rng.sample(currents);
        let subducting = (cell.density > 16000 || cell.height < -2.0) && {
            // cell.height -= 0.1;
            true
        };
        let c = layer.center(i as _);
        let d = cell.motion();
        let neighbors = layer.neighbours(i as _, false);
        for (w, cell) in (0..9).filter_map(|n| {
            Some((
                MainWind::from_index(n),
                *neighbors.get(MainWind::from_index(n))?,
            ))
        }) {
            let cell = &mut terrain[cell as usize];
            if (cell.motion() - d).length_squared() < 0.2 {
                cell.add_lazy_motion(d * 0.25);
            }
            if subducting {
                match w {
                    MainWind::N => cell.dy -= 0.1,
                    MainWind::E => cell.dx -= 0.1,
                    MainWind::S => cell.dy += 0.1,
                    MainWind::W => cell.dx += 0.1,
                    MainWind::NE => {
                        cell.dx -= 0.07;
                        cell.dy -= 0.07;
                    }
                    MainWind::SE => {
                        cell.dx -= 0.07;
                        cell.dy += 0.07;
                    }
                    MainWind::SW => {
                        cell.dx += 0.07;
                        cell.dy += 0.07;
                    }
                    MainWind::NW => {
                        cell.dx += 0.07;
                        cell.dy -= 0.07;
                    }
                    MainWind::C => {}
                }
            }
        }
    }
    for i in 0..terrain.len() {
        let cell = terrain[i];
        let motion = cell.motion();
        let neighbors = layer.neighbours(i as _, false);
        let itertools::MinMaxResult::MinMax((away, adir), (toward, tdir)) = (0..9)
            .filter_map(|i| {
                neighbors
                    .get(MainWind::from_index(i))
                    .map(|&n| (n, WIND_VECS[i as usize]))
            })
            .minmax_by_key(|&(_, v)| v.dot(motion))
        else {
            unreachable!("Each cell should have four neighbors!")
        };
        let t2 = terrain[toward as usize];
        let theight = t2.height;
        let tmotion = t2.motion();
        if motion.dot(tmotion) > 0.5 {
            let collision_size = (motion - tmotion).dot(tdir);
            let cell = &mut terrain[i];
            cell.add_lazy_motion(tdir * collision_size * 0.01);
            if (cell.height - theight).abs() < 0.2 {
                cell.height += collision_size * 0.1;
                terrain[toward as usize].height += collision_size * 0.1;
            } else if cell.height < theight - 0.2 {
                cell.height -= collision_size * 0.1;
                terrain[toward as usize].height += collision_size * 0.05;
            }
        }
        let amotion = terrain[away as usize].motion();
        if motion.dot(amotion) < -0.5 {
            let cell = &mut terrain[i];
            let rift_size = -(motion - amotion).dot(adir);
            let ratio = (rift_size / (1u32 << depth) as f32 * 0.25).clamp(0.0, 1.0);
            cell.age = (cell.age as f32 * (1.0 - ratio)) as u32;
            cell.density = ((cell.density as f32 * (1.0 - ratio)) as u32)
                .saturating_add(rng.sample(density_range) / 10);
        }
    }
    terrain
        .iter_mut()
        .for_each(TectonicCell::resolve_lazy_motion);
    for i in 0..terrain.len() {
        let c = layer.center(i as _);
        // let neighbors = layer.cone_coverage_fullin(c.0, c.1, FRAC_PI_8);
        let neighbors = BMOC::new_empty(depth);
        let height = terrain[i].height;
        let mut count = 2;
        let v = (neighbors
            .into_flat_iter()
            .map(|n| {
                let h = terrain[n as usize].height;
                count += 1;
                if (h - height).abs() > 1.0 {
                    h * 0.4 + height * 0.6
                } else {
                    h
                }
            })
            .sum::<f32>()
            + height * 2.0)
            / (count as f32);
        terrain[i].scratch.float = v;
    }
}
