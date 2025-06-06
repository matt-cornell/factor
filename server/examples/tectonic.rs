use bevy::prelude::*;
use bevy::render::camera::Viewport;
use bevy::render::render_asset::RenderAssetUsages;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat};
use bevy::render::view::RenderLayers;
use bevy::window::WindowResized;
use factor_server::terrain::tectonic::*;
use rand::prelude::*;
use std::f32::consts::*;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, States)]
enum AppState {
    #[default]
    Healpix,
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

#[derive(Debug, Resource, PartialEq)]
enum ColorKind {
    Plates,
    Height,
    Density,
    Feats,
}

/// Are we showing the centers of plates?
#[derive(Resource)]
struct ShowCenters(bool);

/// Are we showing the borders of the plates?
#[derive(Resource)]
struct ShowBorders(bool);

#[derive(Resource)]
struct TerrainData(TectonicState, Box<[LinearRgba]>);

#[derive(Component)]
struct Planet;

#[derive(Component)]
struct MiniMap;

#[derive(Event)]
struct DepthChanged;

#[derive(Event)]
struct RecolorPlates;

struct RandomColor;
impl Distribution<LinearRgba> for RandomColor {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> LinearRgba {
        LinearRgba::rgb(rng.random(), rng.random(), rng.random())
    }
}

const WIDTH: usize = 600;
const HEIGHT: usize = 300;
const VIEW_WIDTH: u32 = 400;
const VIEW_HEIGHT: u32 = 200;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                fit_canvas_to_parent: true,
                title: "Tectonic Terrain".into(),
                ..default()
            }),
            ..default()
        }))
        .init_state::<AppState>()
        .add_sub_state::<Simulating>()
        .insert_resource(HealpixDepth(5))
        .insert_resource(ColorKind::Plates)
        .insert_resource(Rotating(true))
        .insert_resource(ShowCenters(false))
        .insert_resource(ShowBorders(false))
        .insert_resource(Time::<Fixed>::from_hz(5.0))
        .add_event::<DepthChanged>()
        .add_event::<RecolorPlates>()
        .add_systems(Startup, setup)
        .add_systems(PostStartup, update_healpix)
        .add_systems(
            PreUpdate,
            (
                recolor_plates.run_if(on_event::<RecolorPlates>),
                update_healpix.run_if(on_event::<DepthChanged>),
            ),
        )
        .add_systems(
            Update,
            (
                update_map_camera,
                handle_keypresses,
                update_colors.run_if(
                    resource_exists::<TerrainData>.and(
                        resource_changed::<TerrainData>
                            .or(resource_changed::<ColorKind>)
                            .or(resource_changed::<ShowBorders>),
                    ),
                ),
                rotate_sphere.run_if(resource_equals(Rotating(true))),
            ),
        )
        .add_systems(
            FixedUpdate,
            update_terrain
                .before(update_colors)
                .run_if(in_state(Simulating)),
        )
        .add_systems(
            PostUpdate,
            update_texture
                .run_if(resource_changed::<HealpixPixels>.or(resource_changed::<ShowCenters>)),
        )
        .add_systems(
            OnEnter(AppState::Simulate {
                iter: 0,
                running: false,
            }),
            setup_terrain,
        )
        .run();
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    let map_layer = RenderLayers::layer(1);

    commands.spawn((
        Camera3d::default(),
        Transform::from_xyz(0.0, 8.0, 16.0).looking_at(Vec3::new(0.0, 0.0, 0.0), Vec3::Y),
    ));
    commands.spawn((
        Camera2d,
        Camera {
            viewport: Some(Viewport {
                physical_size: UVec2::new(VIEW_WIDTH, VIEW_HEIGHT),
                ..default()
            }),
            order: 1,
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
        Mesh3d(meshes.add(Sphere::new(5.0).mesh().uv(32, 18))),
        MeshMaterial3d(materials.add(StandardMaterial {
            base_color_texture: Some(image.clone()),
            ..default()
        })),
        Transform::from_xyz(0.0, 0.0, 0.0).with_rotation(Quat::from_rotation_x(-FRAC_PI_2)),
        Planet,
    ));
    commands.spawn((
        PointLight {
            shadows_enabled: true,
            intensity: 10_000_000.,
            range: 100.0,
            shadow_depth_bias: 0.2,
            ..default()
        },
        Transform::from_xyz(8.0, 16.0, 8.0),
    ));

    commands.spawn((
        Sprite {
            image: image.clone(),
            ..default()
        },
        Transform::from_scale(Vec3::new(
            VIEW_WIDTH as f32 / WIDTH as f32,
            VIEW_HEIGHT as f32 / HEIGHT as f32,
            1.0,
        )),
        map_layer,
        MiniMap,
    ));
}

fn setup_terrain(mut commands: Commands, depth: Res<HealpixDepth>) {
    let state = init_terrain(depth.0, &mut rand::rng());
    let colors: Box<[LinearRgba]> = rand::rng()
        .sample_iter(RandomColor)
        .take(state.plates().len())
        .collect();
    commands.insert_resource(TerrainData(state, colors));
}

fn recolor_plates(mut terr: ResMut<TerrainData>) {
    terr.1.fill_with(|| rand::rng().sample(RandomColor));
}

fn update_colors(
    terr: Res<TerrainData>,
    mut pixels: ResMut<HealpixPixels>,
    color_kind: Res<ColorKind>,
    bord: Res<ShowBorders>,
    state: Res<State<AppState>>,
) {
    if matches!(state.get(), AppState::Simulate { .. }) {
        let TerrainData(state, colors) = &*terr;
        for (n, (cell, color)) in state.cells().iter().zip(&mut pixels.0).enumerate() {
            if bord.0 && state.boundaries().contains(n as _) {
                *color = LinearRgba::RED;
            } else {
                match *color_kind {
                    ColorKind::Plates => *color = colors[cell.plate as usize],
                    ColorKind::Height => {
                        *color = LinearRgba::gray(cell.height.mul_add(0.1, 0.2));
                    }
                    ColorKind::Density => {
                        let dens = (cell.density as f32).mul_add(0.0025, -0.05);
                        let dens = dens.mul_add(0.2, -0.1);
                        let base = LinearRgba::rgb(0.5 - dens, 0.5, 0.5 + dens);
                        *color = base.with_luminance(cell.height.mul_add(0.1, 0.2));
                    }
                    ColorKind::Feats => {
                        let base = match cell.feats.kind {
                            CellFeatureKind::None => LinearRgba::BLACK,
                            CellFeatureKind::Subduction => LinearRgba::BLUE,
                            CellFeatureKind::Mountain => LinearRgba::GREEN,
                            CellFeatureKind::Ridge => LinearRgba::RED,
                        };
                        *color =
                            base.with_luminance((-(cell.feats.dist as f32 * 0.5).powi(2)).exp());
                    }
                }
            }
        }
    }
}

fn update_terrain(
    mut terr: ResMut<TerrainData>,
    state: Res<State<AppState>>,
    mut next_state: ResMut<NextState<AppState>>,
) {
    step_terrain(&mut terr.0, &mut rand::rng());
    let AppState::Simulate { running, iter } = **state else {
        panic!("Invalid state for terrain update")
    };
    next_state.set(AppState::Simulate {
        running,
        iter: iter + 1,
    });
}

fn handle_keypresses(
    mut commands: Commands,
    keys: Res<ButtonInput<KeyCode>>,
    state: Res<State<AppState>>,
    mut next_state: ResMut<NextState<AppState>>,
    mut depth: ResMut<HealpixDepth>,
    mut rotating: ResMut<Rotating>,
    mut colors: ResMut<ColorKind>,
    mut centers: ResMut<ShowCenters>,
    mut borders: ResMut<ShowBorders>,
    mut depth_evt: EventWriter<DepthChanged>,
    mut recolor_evt: EventWriter<RecolorPlates>,
    mut exit_evt: EventWriter<AppExit>,
) {
    if keys.any_pressed([KeyCode::ControlLeft, KeyCode::ControlRight])
        && keys.just_pressed(KeyCode::KeyW)
    {
        exit_evt.write(AppExit::Success);
    }
    if keys.just_pressed(KeyCode::KeyS) {
        rotating.0 = !rotating.0;
    }
    if keys.just_pressed(KeyCode::KeyP) {
        *colors = ColorKind::Plates;
    }
    if keys.just_pressed(KeyCode::KeyH) {
        *colors = ColorKind::Height;
    }
    if keys.just_pressed(KeyCode::KeyD) {
        *colors = ColorKind::Density;
    }
    if keys.just_pressed(KeyCode::KeyF) {
        *colors = ColorKind::Feats;
    }
    if keys.just_pressed(KeyCode::KeyB) {
        borders.0 = !borders.0;
    }
    if keys.just_pressed(KeyCode::KeyR) {
        commands.remove_resource::<TerrainData>();
        next_state.set(AppState::Healpix);
        depth_evt.write(DepthChanged);
    }
    match *state.get() {
        AppState::Healpix => {
            if keys.just_pressed(KeyCode::Space) {
                next_state.set(AppState::Simulate {
                    iter: 0,
                    running: false,
                });
            } else {
                let mut evt = false;
                if keys.just_pressed(KeyCode::BracketRight) {
                    if depth.0 < 20 {
                        depth.0 += 1;
                    }
                    evt = true;
                }
                if keys.just_pressed(KeyCode::BracketLeft) {
                    if depth.0 > 0 {
                        depth.0 -= 1;
                    }
                    evt = true;
                }
                if evt || keys.just_pressed(KeyCode::KeyC) {
                    depth_evt.write(DepthChanged);
                }
            }
        }
        AppState::Simulate { iter, running } => {
            if keys.just_pressed(KeyCode::Space) {
                next_state.set(AppState::Simulate {
                    iter,
                    running: !running,
                });
            }
            if keys.just_pressed(KeyCode::KeyC) {
                recolor_evt.write(RecolorPlates);
            }
            if keys.just_pressed(KeyCode::KeyX) {
                centers.0 = !centers.0;
            }
        }
    }
}

fn update_healpix(mut commands: Commands, depth: Res<HealpixDepth>) {
    let layer = healpix::get(depth.0);
    let start_data = rand::rng()
        .sample_iter(RandomColor)
        .take(layer.n_hash() as _)
        .collect::<Box<[_]>>();
    let image_map = (0..(WIDTH * HEIGHT))
        .map(|i| {
            use std::f64::consts::*;
            let x = ((i % WIDTH) as f64).mul_add(TAU / WIDTH as f64, -PI);
            let y = ((i / WIDTH) as f64).mul_add(-PI / HEIGHT as f64, FRAC_PI_2);
            layer.hash([x, y]) as usize
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
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut planet: Single<&mut MeshMaterial3d<StandardMaterial>, With<Planet>>,
    mut minimap: Single<&mut Sprite, With<MiniMap>>,
    terrain: Option<Res<TerrainData>>,
    show_centers: Res<ShowCenters>,
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
    'pixels: for (n, d) in bytemuck::cast_slice_mut(img.data.as_mut().unwrap())
        .iter_mut()
        .enumerate()
    {
        if show_centers.0 {
            if let Some(r) = &terrain {
                let TerrainData(state, colors) = &**r;
                let mut black = false;
                let cx = ((n % WIDTH) as f32).mul_add(TAU / WIDTH as f32, -PI);
                let cy = ((n / WIDTH) as f32).mul_add(-PI / HEIGHT as f32, FRAC_PI_2);
                for (plate, color) in state.plates().iter().zip(colors) {
                    let sqdist = Vec2::new(cx, cy).distance_squared(plate.center.as_f32s().into());
                    if sqdist < 0.005 {
                        *d = color.as_u32();
                        continue 'pixels;
                    } else if sqdist < 0.015 {
                        black = true;
                    }
                }
                if black {
                    *d = LinearRgba::BLACK.as_u32();
                    continue 'pixels;
                }
            }
        }
        *d = data.0[map.0[n]].as_u32();
    }
    let image = images.add(img);
    planet.0 = materials.add(StandardMaterial {
        base_color_texture: Some(image.clone()),
        ..default()
    });
    minimap.image = image;
}

fn rotate_sphere(mut query: Single<&mut Transform, With<Planet>>, time: Res<Time>) {
    query.rotate_y(time.delta_secs() / 2.0);
}
