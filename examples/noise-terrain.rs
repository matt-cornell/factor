#![allow(clippy::too_many_arguments)]
use bevy::prelude::*;
use bevy::render::camera::Viewport;
use bevy::render::render_asset::RenderAssetUsages;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat};
use bevy::render::view::RenderLayers;
use bevy::window::WindowResized;
use bevy_egui::{egui, EguiContexts, EguiPlugin};
use factor::terrain::noise::*;
use rand::prelude::*;
use std::f32::consts::*;
use std::sync::atomic::{AtomicBool, Ordering};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, States)]
enum AppState {
    #[default]
    Heights,
}

/// Currently rotating
#[derive(Resource, PartialEq)]
struct Rotating(bool);

#[derive(Resource)]
struct ShowOceans {
    show: bool,
    depth: f32,
}

#[derive(Resource)]
struct LayerFilter(Option<usize>);

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

#[derive(Debug, Default)]
struct NoiseSourceBuilder {
    gradient: bool,
    depth: u8,
    shift: f32,
    scale: f32,
    changed: AtomicBool,
}
impl NoiseSourceBuilder {
    pub const fn value(depth: u8, shift: f32, scale: f32) -> Self {
        Self {
            gradient: false,
            depth,
            shift,
            scale,
            changed: AtomicBool::new(true),
        }
    }
    pub const fn gradient(depth: u8, shift: f32, scale: f32) -> Self {
        Self {
            gradient: true,
            depth,
            shift,
            scale,
            changed: AtomicBool::new(true),
        }
    }
    pub fn build(&self) -> (Shifted<ValueOrGradient>, f32) {
        self.changed.store(false, Ordering::Relaxed);
        (
            cell_noise(self.gradient, self.depth, self.shift),
            self.scale,
        )
    }
}
impl Clone for NoiseSourceBuilder {
    fn clone(&self) -> Self {
        Self {
            changed: AtomicBool::new(true),
            ..*self
        }
    }
}

fn make_noise<'a, I: IntoIterator<Item = &'a NoiseSourceBuilder>>(
    iter: I,
) -> Vec<(Shifted<ValueOrGradient>, f32)> {
    iter.into_iter().map(NoiseSourceBuilder::build).collect()
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(EguiPlugin)
        .init_state::<AppState>()
        .add_event::<ReloadTerrain>()
        .insert_resource(Rotating(true))
        .insert_resource(LayerFilter(None))
        .insert_resource(ShowOceans {
            show: false,
            depth: 0.5,
        })
        .insert_resource(NoiseSourceRes(vec![
            NoiseSourceBuilder::value(1, 0.0, 0.15),
            NoiseSourceBuilder::value(1, 0.1, 0.15),
            NoiseSourceBuilder::value(2, 0.2, 0.294),
            NoiseSourceBuilder::value(2, 0.3, 0.294),
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
                update_ui,
                rotate_sphere.run_if(resource_equals(Rotating(true))),
                update_texture.run_if(
                    state_changed::<AppState>
                        .or_else(resource_changed::<NoiseTerrain>)
                        .or_else(resource_changed::<ShowOceans>)
                        .or_else(resource_changed::<LayerFilter>),
                ),
                update_noise_terrain.run_if(resource_changed::<NoiseSourceRes>),
                reload_terrain.run_if(on_event::<ReloadTerrain>()),
            ),
        )
        .add_systems(OnEnter(AppState::Heights), reload_terrain)
        .run();
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    window: Query<&Window>,
) {
    let map_layer = RenderLayers::layer(1);

    let window = window.single();

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
                    physical_position: UVec2::new(
                        window.width() as u32 - VIEW_WIDTH,
                        window.height() as u32 - VIEW_HEIGHT,
                    ),
                    ..default()
                }),
                order: 1,
                ..default()
            },
            ..default()
        },
        map_layer.clone(),
        MiniMap,
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
        oceans.show = !oceans.show;
    }
    match *state.get() {
        AppState::Heights => {
            if keys.just_pressed(KeyCode::KeyR) {
                reroll_rand.send(ReloadTerrain);
            }
        }
    }
}

fn update_ui(
    mut contexts: EguiContexts,
    mut rotating: ResMut<Rotating>,
    mut oceans: ResMut<ShowOceans>,
    mut filter: ResMut<LayerFilter>,
    mut noise: ResMut<NoiseSourceRes>,
    mut terrain: ResMut<NoiseTerrain>,
    mut reroll_rand: EventWriter<ReloadTerrain>,
    mut wip_layer: Local<Option<NoiseSourceBuilder>>,
) {
    egui::SidePanel::left("controls")
        .resizable(true)
        .show(contexts.ctx_mut(), |ui| {
            ui.set_min_width(200.0);
            ui.label(egui::RichText::new("Controls").size(20.0));
            if ui.button("Restart").clicked() {
                reroll_rand.send(ReloadTerrain);
            }
            if ui
                .checkbox(&mut rotating.bypass_change_detection().0, "Rotating")
                .changed()
            {
                let _ = &mut *rotating;
            }
            ui.collapsing(egui::RichText::new("Oceans").size(18.0), |ui| {
                if ui
                    .checkbox(&mut oceans.bypass_change_detection().show, "Show Oceans")
                    .changed()
                {
                    let _ = &mut *oceans;
                }
                if ui
                    .add(egui::Slider::new(
                        &mut oceans.bypass_change_detection().depth,
                        0.0..=1.0,
                    ))
                    .changed()
                {
                    let _ = &mut *oceans;
                }
            });
            ui.collapsing(egui::RichText::new("Noise").size(18.0), |ui| {
                let mut showing = None;
                let mut changed = false;
                let mut delete = None;
                let popup_id = egui::Id::new("new-layer");
                let new_button = ui.button("New");
                if new_button.clicked() {
                    ui.memory_mut(|mem| mem.open_popup(popup_id));
                }
                egui::popup_above_or_below_widget(
                    ui,
                    popup_id,
                    &new_button,
                    egui::AboveOrBelow::Below,
                    egui::PopupCloseBehavior::CloseOnClickOutside,
                    |ui| {
                        ui.set_min_width(100.0);
                        let layer = wip_layer.get_or_insert_default();
                        ui.add(egui::Slider::new(&mut layer.depth, 0..=8).text("Layer"));
                        ui.add(egui::Slider::new(&mut layer.scale, 0.0..=3.0).text("Scale"));
                        ui.add(egui::Slider::new(&mut layer.shift, -0.5..=0.5).text("Shift"));
                        ui.checkbox(&mut layer.gradient, "Gradient");
                        ui.horizontal(|ui| {
                            if ui.button("Add").clicked() {
                                noise.0.push(wip_layer.take().unwrap());
                            }
                            if ui.button("Cancel").clicked() {
                                *wip_layer = None;
                            }
                        })
                    },
                );

                for (n, layer) in noise.bypass_change_detection().0.iter_mut().enumerate() {
                    let frame = egui::Frame::group(ui.style()).show(ui, |ui| {
                        ui.set_min_width(125.0);
                        ui.label(format!(
                            "Layer: {}\nScale: {}\nShift: {}\nGradient: {}",
                            layer.depth, layer.scale, layer.shift, layer.gradient
                        ));
                    });
                    frame.response.on_hover_ui(|ui| {
                        showing = Some(n);
                        egui::Frame::popup(ui.style()).show(ui, |ui| {
                            *layer.changed.get_mut() |= ui
                                .add(egui::Slider::new(&mut layer.depth, 0..=8).text("Layer"))
                                .changed();
                            if ui
                                .add(egui::Slider::new(&mut layer.scale, 0.0..=3.0).text("Scale"))
                                .changed()
                            {
                                changed = true; // don't count this as a change because it's lazy
                                terrain.0[n].1 = layer.scale;
                            }
                            if ui
                                .add(egui::Slider::new(&mut layer.shift, 0.0..=3.0).text("Scale"))
                                .changed()
                            {
                                changed = true; // don't count this as a change because it's lazy
                                terrain.0[n].0.shift = layer.shift;
                            }
                            *layer.changed.get_mut() |=
                                ui.checkbox(&mut layer.gradient, "Gradient").changed();
                            ui.horizontal(|ui| {
                                if ui.button("Reload").clicked() {
                                    *layer.changed.get_mut() = true;
                                }
                                if ui.button("Delete").clicked() {
                                    delete = Some(n);
                                    showing = None;
                                }
                            });
                            changed |= *layer.changed.get_mut();
                        });
                    });
                }
                if filter.0 != showing {
                    // avoid an update if we can
                    filter.0 = showing;
                }
                if let Some(idx) = delete {
                    noise.0.remove(idx);
                }
                if changed {
                    let _ = &mut *noise;
                }
            });
        });
}

fn update_map_camera(
    windows: Query<&Window>,
    mut resize_events: EventReader<WindowResized>,
    mut query: Query<(&mut Camera, Has<MiniMap>), With<Camera2d>>,
) {
    for resize_event in resize_events.read() {
        let window = windows.get(resize_event.window).unwrap();
        let win_size = window.size();
        let size = UVec2::new(VIEW_WIDTH, VIEW_HEIGHT);

        for (mut camera, is_map) in &mut query {
            if is_map {
                camera.viewport = Some(Viewport {
                    physical_position: win_size.as_uvec2() - size,
                    physical_size: size,
                    ..default()
                });
            }
        }
    }
}

fn reload_terrain(mut commands: Commands, noise: Res<NoiseSourceRes>) {
    commands.insert_resource(NoiseTerrain(make_noise(noise.0.iter())));
}

fn update_noise_terrain(noise: Res<NoiseSourceRes>, mut terr: ResMut<NoiseTerrain>) {
    let mut changed = false;
    for (b, n) in noise.0.iter().zip(&mut terr.bypass_change_detection().0) {
        if b.changed.load(Ordering::Relaxed) {
            *n = b.build();
            changed = true;
        }
    }
    if changed {
        let _ = &mut *terr;
    }
}

fn update_texture(
    state: Res<State<AppState>>,
    noise: Res<NoiseTerrain>,
    oceans: Res<ShowOceans>,
    filter: Res<LayerFilter>,
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
        let height = if let Some(idx) = filter.0 {
            noise.0[idx].get_height(x, y)
        } else {
            noise.0.get_height(x, y)
        };
        match **state {
            AppState::Heights => {
                *d = if filter.0.is_none() && oceans.show && height < oceans.depth {
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
