#![allow(clippy::too_many_arguments)]
use bevy::prelude::*;
use bevy::render::camera::Viewport;
use bevy::render::render_asset::RenderAssetUsages;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat};
use bevy::render::view::RenderLayers;
use bevy::window::WindowResized;
use factor::terrain::noise::*;
use rand::prelude::*;
use std::f32::consts::*;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, States)]
enum AppState {
    #[default]
    Healpix,
    Heights,
}

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
struct NoiseSourceRes(Box<dyn NoiseSource + Send + Sync>);

#[derive(Resource)]
struct TerrainData();

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
const VIEW_WIDTH: u32 = 400;
const VIEW_HEIGHT: u32 = 200;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .init_state::<AppState>()
        .insert_resource(HealpixDepth(5))
        .insert_resource(Rotating(true))
        .insert_resource(NoiseSourceRes(Box::new(ValueNoise::new(
            3,
            // Oracle::new(|| thread_rng().gen()),
            |lon, lat| (lon + lat) * FRAC_1_PI % 1.0,
            |a0, a1, w| (a1 - a0) * w + a0,
        ))))
        .add_event::<DepthChanged>()
        .add_systems(Startup, (setup, update_healpix.after(setup)))
        .add_systems(PreUpdate, update_healpix.run_if(on_event::<DepthChanged>()))
        .add_systems(
            Update,
            (
                update_map_camera,
                handle_keypresses,
                rotate_sphere.run_if(resource_equals(Rotating(true))),
            ),
        )
        .add_systems(
            PostUpdate,
            update_texture.run_if(
                resource_changed::<HealpixPixels>
                    .or_else(resource_exists_and_changed::<NoiseSourceRes>),
            ),
        )
        .add_systems(OnEnter(AppState::Heights), update_texture)
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
                    physical_size: UVec2::new(VIEW_WIDTH, VIEW_HEIGHT),
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
            mesh: meshes.add(Sphere::new(5.0).mesh().uv(32, 18)),
            material: materials.add(StandardMaterial {
                base_color_texture: Some(image.clone()),
                ..default()
            }),
            transform: Transform::from_xyz(0.0, 0.0, 0.0)
                .with_rotation(Quat::from_rotation_x(-FRAC_PI_2)),
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
            transform: Transform::from_scale(Vec3::new(
                VIEW_WIDTH as f32 / WIDTH as f32,
                VIEW_HEIGHT as f32 / HEIGHT as f32,
                1.0,
            )),
            ..default()
        },
        map_layer,
        MiniMap,
    ));
}
fn handle_keypresses(
    mut commands: Commands,
    keys: Res<ButtonInput<KeyCode>>,
    state: Res<State<AppState>>,
    mut next_state: ResMut<NextState<AppState>>,
    mut depth: ResMut<HealpixDepth>,
    mut rotating: ResMut<Rotating>,
    mut depth_evt: EventWriter<DepthChanged>,
    mut exit_evt: EventWriter<AppExit>,
) {
    if keys.any_pressed([KeyCode::ControlLeft, KeyCode::ControlRight])
        && keys.just_pressed(KeyCode::KeyW)
    {
        exit_evt.send(AppExit::Success);
    }
    if keys.just_pressed(KeyCode::KeyS) {
        rotating.0 = !rotating.0;
    }
    if keys.just_pressed(KeyCode::KeyR) {
        commands.remove_resource::<TerrainData>();
        next_state.set(AppState::Healpix);
        depth_evt.send(DepthChanged);
    }
    match *state.get() {
        AppState::Healpix => {
            if keys.just_pressed(KeyCode::Space) {
                next_state.set(AppState::Heights);
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
                if evt || keys.just_pressed(KeyCode::KeyC) {
                    depth_evt.send(DepthChanged);
                }
            }
        }
        AppState::Heights => {
            if keys.just_pressed(KeyCode::Space) {
                // TODO: next state
            }
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
            let x = (i % WIDTH) as f64 * TAU / WIDTH as f64;
            let y = ((i / WIDTH) as f64).mul_add(-PI / HEIGHT as f64, FRAC_PI_2);
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
        let size = UVec2::new(VIEW_WIDTH, VIEW_HEIGHT);

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
    state: Res<State<AppState>>,
    noise: Option<Res<NoiseSourceRes>>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut planet: Query<&mut Handle<StandardMaterial>, With<Planet>>,
    mut minimap: Query<&mut Handle<Image>, With<MiniMap>>,
) {
    let mut img = Image::new(
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
    for (n, d) in bytemuck::cast_slice_mut(&mut img.data)
        .iter_mut()
        .enumerate()
    {
        match **state {
            AppState::Healpix => *d = data.0[map.0[n]].as_u32(),
            AppState::Heights => {
                use std::f32::consts::*;
                let x = ((n % WIDTH) as f32) * TAU / WIDTH as f32;
                let y = ((n / WIDTH) as f32).mul_add(-PI / HEIGHT as f32, FRAC_PI_2);
                let height = noise.as_ref().unwrap().0.get_height(x, y);
                *d = LinearRgba::gray(height).as_u32();
            }
        }
    }
    let image = images.add(img);
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
