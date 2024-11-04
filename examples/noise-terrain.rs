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
    Heights,
}

/// Currently rotating
#[derive(Resource, PartialEq)]
struct Rotating(bool);

#[derive(Resource)]
struct ShowOceans(bool);

#[derive(Resource)]
struct NoiseSourceRes(Vec<NoiseSourceBuilder>);

#[derive(Resource)]
struct NoiseTerrain(Vec<(Shifted<ValueOrGradient>, f32)>);

#[derive(Component)]
struct Planet;

#[derive(Component)]
struct MiniMap;

#[derive(Event)]
struct ReloadTerrain;

const WIDTH: usize = 400;
const HEIGHT: usize = 200;
const VIEW_WIDTH: u32 = 400;
const VIEW_HEIGHT: u32 = 200;

fn smoothstep(w: f32) -> f32 {
    (3.0 - w * 2.0) * w * w
}

#[allow(clippy::type_complexity)]
#[derive(Debug, Clone)]
enum ValueOrGradient {
    Value(ValueCellNoise<Box<[f32]>, fn(f32) -> f32>),
    Gradient(GradientCellNoise<Box<[Vec2]>, fn(f32) -> f32>),
}
impl NoiseSource for ValueOrGradient {
    fn get_height(&self, lon: f32, lat: f32) -> f32 {
        match self {
            Self::Value(v) => v.get_height(lon, lat),
            Self::Gradient(g) => g.get_height(lon, lat),
        }
    }
}

fn cell_noise(gradient: bool, depth: u8, shift: f32) -> Shifted<ValueOrGradient> {
    let base = if gradient {
        ValueOrGradient::Gradient(GradientCellNoise {
            depth,
            hasher: thread_rng()
                .sample_iter(rand_distr::UnitCircle)
                .map(|[x, y]| Vec2::new(x, y))
                .take(factor::healpix::n_hash(depth) as _)
                .collect::<Box<[Vec2]>>(),
            scale: smoothstep,
        })
    } else {
        ValueOrGradient::Value(ValueCellNoise {
            depth,
            hasher: thread_rng()
                .sample_iter(rand_distr::Standard)
                .take(factor::healpix::n_hash(depth) as _)
                .collect::<Box<[f32]>>(),
            scale: smoothstep,
        })
    };
    Shifted::new(base, shift)
}

#[derive(Debug, Clone, Copy)]
struct NoiseSourceBuilder {
    gradient: bool,
    depth: u8,
    shift: f32,
    scale: f32,
}
impl NoiseSourceBuilder {
    pub const fn value(depth: u8, shift: f32, scale: f32) -> Self {
        Self {
            gradient: false,
            depth,
            shift,
            scale,
        }
    }
    pub const fn gradient(depth: u8, shift: f32, scale: f32) -> Self {
        Self {
            gradient: true,
            depth,
            shift,
            scale,
        }
    }
    pub fn build(self) -> (Shifted<ValueOrGradient>, f32) {
        (
            cell_noise(self.gradient, self.depth, self.shift),
            self.scale,
        )
    }
}
fn make_noise<I: IntoIterator<Item = NoiseSourceBuilder>>(
    iter: I,
) -> Vec<(Shifted<ValueOrGradient>, f32)> {
    iter.into_iter().map(NoiseSourceBuilder::build).collect()
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .init_state::<AppState>()
        .add_event::<ReloadTerrain>()
        .insert_resource(Rotating(true))
        .insert_resource(ShowOceans(false))
        .insert_resource(NoiseSourceRes(vec![
            NoiseSourceBuilder::value(1, 0.0, 0.05),
            NoiseSourceBuilder::value(1, 0.1, 0.05),
            NoiseSourceBuilder::value(2, 0.2, 0.394),
            NoiseSourceBuilder::value(2, 0.3, 0.394),
            NoiseSourceBuilder::value(3, 0.5, 0.05),
            NoiseSourceBuilder::value(3, 0.7, 0.05),
            NoiseSourceBuilder::gradient(5, 0.0, 0.01),
        ]))
        .add_systems(Startup, setup)
        .add_systems(
            Update,
            (
                update_map_camera,
                handle_keypresses,
                rotate_sphere.run_if(resource_equals(Rotating(true))),
                update_texture.run_if(
                    resource_changed::<NoiseTerrain>
                        .or_else(state_changed::<AppState>)
                        .or_else(resource_changed::<ShowOceans>),
                ),
                update_terrain.run_if(on_event::<ReloadTerrain>()),
            ),
        )
        .add_systems(OnEnter(AppState::Heights), update_terrain)
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
    // mut commands: Commands,
    keys: Res<ButtonInput<KeyCode>>,
    state: Res<State<AppState>>,
    mut rotating: ResMut<Rotating>,
    mut oceans: ResMut<ShowOceans>,
    mut reroll_rand: EventWriter<ReloadTerrain>,
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
    if keys.just_pressed(KeyCode::KeyO) {
        oceans.0 = !oceans.0;
    }
    match *state.get() {
        AppState::Heights => {
            if keys.just_pressed(KeyCode::KeyR) {
                reroll_rand.send(ReloadTerrain);
            }
        }
    }
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

fn update_terrain(mut commands: Commands, noise: Res<NoiseSourceRes>) {
    commands.insert_resource(NoiseTerrain(make_noise(noise.0.iter().copied())));
}

fn update_texture(
    state: Res<State<AppState>>,
    noise: Res<NoiseTerrain>,
    oceans: Res<ShowOceans>,
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
        use std::f32::consts::*;
        let x = ((n % WIDTH) as f32) * TAU / WIDTH as f32;
        let y = ((n / WIDTH) as f32).mul_add(-PI / HEIGHT as f32, FRAC_PI_2);
        let height = noise.0.get_height(x, y);
        match **state {
            AppState::Heights => {
                *d = if oceans.0 && height < 0.5 {
                    LinearRgba::from(Srgba::rgb_u8(0, 51, 102)).with_luminance(height * 0.5)
                } else {
                    LinearRgba::gray(height)
                }
                .as_u32();
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
