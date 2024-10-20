#![allow(clippy::too_many_arguments)]

use bevy::prelude::*;
use bevy::render::camera::Viewport;
use bevy::render::render_asset::RenderAssetUsages;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat};
use bevy::render::view::RenderLayers;
use bevy::window::WindowResized;
use factor::terrain::*;
use rand::prelude::*;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, States)]
enum AppState {
    #[default]
    Healpix,
    Init,
    Simulate {
        iter: u16,
        running: bool,
    },
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, SubStates)]
#[source(AppState = AppState::Simulate { running: true, .. })]
struct Simulating;

/// Depth to use
#[derive(Resource)]
struct HealpixDepth(u8);

/// Mapping from pixel to corresponding section
#[derive(Resource)]
struct HealpixMap(Box<[usize]>);

/// Image data
#[derive(Resource)]
struct HealpixPixels(Box<[LinearRgba]>);

/// Currently rotating
#[derive(Resource, PartialEq)]
struct Rotating(bool);

#[derive(Resource)]
struct TerrainData(Box<[TectonicCell]>);

#[derive(Component)]
struct Planet;

#[derive(Component)]
struct MiniMap;

#[derive(Event)]
struct DepthChanged;

struct RandomColor;
impl Distribution<LinearRgba> for RandomColor {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> LinearRgba {
        LinearRgba::rgb(rng.gen(), rng.gen(), rng.gen())
    }
}

const WIDTH: usize = 400;
const HEIGHT: usize = 200;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .init_state::<AppState>()
        .add_sub_state::<Simulating>()
        .insert_resource(HealpixDepth(2))
        .insert_resource(Rotating(true))
        .insert_resource(Time::<Fixed>::from_hz(20.0))
        .add_event::<DepthChanged>()
        .add_systems(Startup, setup)
        .add_systems(PostStartup, update_healpix)
        .add_systems(PreUpdate, update_healpix.run_if(on_event::<DepthChanged>()))
        .add_systems(
            Update,
            (
                update_map_camera,
                handle_keypresses,
                rotate_sphere.run_if(resource_equals(Rotating(true))),
            ),
        )
        .add_systems(FixedUpdate, update_terrain.run_if(in_state(Simulating)))
        .add_systems(
            PostUpdate,
            update_texture.run_if(resource_changed::<HealpixPixels>),
        )
        .add_systems(OnEnter(AppState::Init), setup_terrain)
        .run();
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    let map_layer = RenderLayers::layer(1);

    commands.spawn(Camera3dBundle {
        transform: Transform::from_xyz(0.0, 8.0, 16.0)
            .looking_at(Vec3::new(0.0, 0.0, 0.0), Vec3::Y),
        ..default()
    });
    commands.spawn((
        Camera2dBundle {
            camera: Camera {
                viewport: Some(Viewport {
                    physical_size: UVec2::new(WIDTH as _, HEIGHT as _),
                    ..default()
                }),
                order: 1,
                ..default()
            },
            ..default()
        },
        map_layer.clone(),
    ));

    let image = images.add(Image::new(
        Extent3d {
            width: WIDTH as _,
            height: HEIGHT as _,
            depth_or_array_layers: 1,
        },
        TextureDimension::D2,
        vec![0; WIDTH * HEIGHT * 4],
        TextureFormat::Rgba8Unorm,
        RenderAssetUsages::RENDER_WORLD,
    ));

    commands.spawn((
        PbrBundle {
            mesh: meshes.add(Sphere::new(5.0)),
            material: materials.add(StandardMaterial {
                base_color_texture: Some(image.clone()),
                ..default()
            }),
            transform: Transform::from_xyz(0.0, 0.0, 0.0).with_rotation(Quat::from_rotation_x(0.0)),
            ..default()
        },
        Planet,
    ));
    commands.spawn(PointLightBundle {
        point_light: PointLight {
            shadows_enabled: true,
            intensity: 10_000_000.,
            range: 100.0,
            shadow_depth_bias: 0.2,
            ..default()
        },
        transform: Transform::from_xyz(8.0, 16.0, 8.0),
        ..default()
    });

    commands.spawn((
        SpriteBundle {
            texture: image.clone(),
            ..default()
        },
        map_layer,
        MiniMap,
    ));
}

fn setup_terrain(
    mut commands: Commands,
    depth: Res<HealpixDepth>,
    mut pixels: ResMut<HealpixPixels>,
) {
    let data = init_terrain(depth.0, &mut thread_rng());
    for (cell, color) in data.iter().zip(&mut pixels.0) {
        color.red = ((cell.density as f32).log10() - 4.2).clamp(0.0, 1.0);
        color.green = cell.height * 2.0 + 0.3;
        color.blue = ((cell.age as f32).log10() - 5.3).clamp(0.0, 1.0);
    }
    commands.insert_resource(TerrainData(data));
}

fn update_terrain(
    depth: Res<HealpixDepth>,
    mut terr: ResMut<TerrainData>,
    mut pixels: ResMut<HealpixPixels>,
) {
    step_terrain(depth.0, &mut terr.0, &mut thread_rng());
    for (cell, color) in terr.0.iter().zip(&mut pixels.0) {
        color.red = ((cell.density as f32).log10() - 4.2).clamp(0.0, 1.0);
        color.green = cell.height * 2.0 + 0.3;
        color.blue = ((cell.age as f32).log10() - 5.3).clamp(0.0, 1.0);
    }
}

fn handle_keypresses(
    keys: Res<ButtonInput<KeyCode>>,
    mut depth: ResMut<HealpixDepth>,
    mut rotating: ResMut<Rotating>,
    state: Res<State<AppState>>,
    mut next_state: ResMut<NextState<AppState>>,
    mut events: ResMut<Events<DepthChanged>>,
) {
    if keys.just_pressed(KeyCode::KeyR) {
        rotating.0 = !rotating.0;
    }
    match *state.get() {
        AppState::Healpix => {
            if keys.just_pressed(KeyCode::Space) {
                next_state.set(AppState::Init);
            } else {
                let mut evt = false;
                if keys.just_pressed(KeyCode::KeyP) {
                    if depth.0 < 20 {
                        depth.0 += 1;
                    }
                    evt = true;
                }
                if keys.just_pressed(KeyCode::KeyL) {
                    if depth.0 > 0 {
                        depth.0 -= 1;
                    }
                    evt = true;
                }
                if evt {
                    events.send(DepthChanged);
                }
            }
        }
        AppState::Init => {
            if keys.just_pressed(KeyCode::Space) {
                next_state.set(AppState::Simulate {
                    iter: 0,
                    running: true,
                });
            }
        }
        AppState::Simulate { iter, running } => {
            next_state.set(AppState::Simulate {
                iter,
                running: !running,
            });
        }
    }
}

fn update_healpix(mut commands: Commands, depth: Res<HealpixDepth>) {
    let start_data = thread_rng()
        .sample_iter(RandomColor)
        .take(cdshealpix::n_hash(depth.0) as _)
        .collect::<Box<[_]>>();
    let image_map = (0..(WIDTH * HEIGHT))
        .map(|i| {
            use std::f64::consts::*;
            let x = ((i % WIDTH) as f64).mul_add(TAU / WIDTH as f64, -PI);
            let y = ((i / WIDTH) as f64).mul_add(PI / HEIGHT as f64, -FRAC_PI_2);
            cdshealpix::nested::hash(depth.0, x, y) as usize
        })
        .collect();

    commands.insert_resource(HealpixMap(image_map));
    commands.insert_resource(HealpixPixels(start_data));
}

fn update_map_camera(
    windows: Query<&Window>,
    mut resize_events: EventReader<WindowResized>,
    mut query: Query<&mut Camera, With<Camera2d>>,
) {
    for resize_event in resize_events.read() {
        let window = windows.get(resize_event.window).unwrap();
        let win_size = window.size();
        let size = UVec2::new(WIDTH as _, HEIGHT as _);

        for mut camera in &mut query {
            camera.viewport = Some(Viewport {
                physical_position: win_size.as_uvec2() - size,
                physical_size: size,
                ..default()
            });
        }
    }
}

fn update_texture(
    map: Res<HealpixMap>,
    data: Res<HealpixPixels>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut planet: Query<&mut Handle<StandardMaterial>, With<Planet>>,
    mut minimap: Query<&mut Handle<Image>, With<MiniMap>>,
) {
    let mut image = Image::new(
        Extent3d {
            width: WIDTH as _,
            height: HEIGHT as _,
            depth_or_array_layers: 1,
        },
        TextureDimension::D2,
        vec![0; WIDTH * HEIGHT * 4],
        TextureFormat::Rgba8Unorm,
        RenderAssetUsages::RENDER_WORLD,
    );
    for (n, d) in bytemuck::cast_slice_mut(&mut image.data)
        .iter_mut()
        .enumerate()
    {
        *d = data.0[map.0[n]].as_u32();
    }
    let image = images.add(image);
    *planet.single_mut() = materials.add(StandardMaterial {
        base_color_texture: Some(image.clone()),
        ..default()
    });
    *minimap.single_mut() = image;
}

fn rotate_sphere(mut query: Query<&mut Transform, With<Planet>>, time: Res<Time>) {
    let mut trans = query.single_mut();
    trans.rotate_y(time.delta_seconds() / 2.0);
}
