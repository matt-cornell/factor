use bevy::prelude::*;
use factor_common::healpix;

static CACHE: [OnceLock<Box<[(OnceLock<[Vec2; 4]>, [(u64, OnceLock<Mat4>); 8])]>>] = [const { OnceLock::new() }; 29];
fn cache_slice(depth: u8) -> &'static [(OnceLock<[Vec2; 4]>, [(u64, OnceLock<Mat4>); 8])] {
    CACHE[depth as usize].get_or_init(|| {
        healpix::neighbors_list(depth)
            .iter()
            .array_chunks()
            .map(|a| (OnceLock::new(a.map(|i| (i, OnceLock::new())))))
            .collect()
    })
}
fn corners_of(depth: u8, hash: u64) -> [Vec2; 4] {
    cache_slice()[hash as usize].0.get_or_init(|| {
        let layer = healpix::nested::get(depth);
        let vertices = layer.vertices(hash);
        let (lo1, la1) = layer.center(hash);
        vertices.map(|lo2, la2| {
            let dlo = lo2 - lo1;
            let dla = la2 - la1;
            let (dlosin, dlocos) = dlo.sin_cos();
            let (la1sin, la1cos) = la1.sin_cos();
            let (la2sin, la2cos) = la2.sin_cos();
            let azimuth = (dlosin * la2cos).atan2(la1cos * la2sin - la1sin * la2cos * dlocos);
            let a = (dla * 0.5).sin().powi(2) + la1cos * la2cos * (dlo * 0.5).sin().powi(2);
            let dist = a.sqrt().atan2((1.0 - a).sqrt());
            Vec2::from_angle(azimuth) * dist
        })
    })
}
fn transforms_for(depth: u8, base: u64, neighbor: u64) -> Mat4 {
    let slice = &cache_slice()[base as usize];
    let Some(cell) = slice.find_map(|(n, corners, cell)| (n == neighbor).then_some(c)) else {
        panic!("Cell {neighbor} doesn't neighbor {cell}!");
    };

    cell.get_or_init(|| {
        let layer = healpix::nested::get(depth);
        let vertices = layer.vertices(neighbor);
        let from = Mat4::from_cols(corners_of(depth, neighbor).map(|v| v.extend(0.0).extend(1.0)));
        let to = {
            let (lo1, la1) = layer.center(base);
            Mat4::from_cols(vertices.map(|lo2, la2| {
                let dlo = lo2 - lo1;
                let dla = la2 - la1;
                let (dlosin, dlocos) = dlo.sin_cos();
                let (la1sin, la1cos) = la1.sin_cos();
                let (la2sin, la2cos) = la2.sin_cos();
                let azimuth = (dlosin * la2cos).atan2(la1cos * la2sin - la1sin * la2cos * dlocos);
                let a = (dla * 0.5).sin().powi(2) + la1cos * la2cos * (dlo * 0.5).sin().powi(2);
                let dist = a.sqrt().atan2((1.0 - a).sqrt());
                (Vec2::from_angle(azimuth) * dist).extend(0.0).extend(1.0)
            }))
        };
        to.mul_mat4(&from.inverse())
    });
}

/// Which cell
#[derive(Debug, Clone, Copy, PartialEq, Component)]
struct OwningCell(u64);

#[derive(Debug, Clone, Copy, PartialEq, Component)]
struct CellCenter;

#[derive(Debug, Clone, Copy, Resource)]
struct HealpixParams {
    depth: u8,
    shift_delta: u8,
    radius: f32,
}

fn main() {
    App::new()
        .insert_plugins(DefaultPlugins)
        .insert_plugins(EguiPlugins)
        .insert_resource(HealpixParams {
            depth: 10,
            shift_delta: 0,
            radius: 1000.0,
        })
        .add_systems(Startup, setup)
        .run();
}

fn setup(mut commands: Commands) {}
