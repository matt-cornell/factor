#![allow(clippy::too_many_arguments, clippy::type_complexity)]
use bevy::input::mouse::*;
use bevy::prelude::*;
use bevy::render::render_asset::RenderAssetUsages;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat};
use bevy::window::PrimaryWindow;
use bevy_egui::{egui, EguiContexts, EguiPlugin};
use factor::terrain::noise::*;
use factor::terrain::tectonic::*;
use rand::prelude::*;
use serde::{Deserialize, Serialize};
use std::cell::UnsafeCell;
use std::f32::consts::*;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, OnceLock};
#[cfg(not(target_arch = "wasm32"))]
use std::time::{Duration, Instant};
#[cfg(target_arch = "wasm32")]
use web_time::{Duration, Instant};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, States)]
enum AppState {
    Tectonics { iter: u16, running: bool },
}
impl Default for AppState {
    fn default() -> Self {
        Self::Tectonics {
            iter: 0,
            running: false,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, SubStates)]
#[source(AppState = AppState::Tectonics { running: true, .. })]
struct Simulating;

#[derive(Resource)]
struct ShowOceans {
    show: bool,
    depth: f32,
}

/// Depth to use for tectonics
#[derive(Resource)]
struct TectonicDepth(u8);

/// Mapping from pixel to corresponding section
#[derive(Resource)]
struct TectonicMap(Box<[usize]>);

#[derive(Resource)]
struct NoiseMap(Box<[usize]>);

/// Heights to use for noise sampling
#[derive(Resource)]
struct NoiseHeights(Box<[f32]>);

/// Total computed heights
#[derive(Resource)]
struct TotalHeights(Box<[f32]>);

#[derive(Default, Clone, Copy, Resource, PartialEq)]
enum LayerFilter {
    #[default]
    All,
    Tectonics,
    AllNoise,
    NoiseLayer(usize),
}

/// Are we showing the centers of plates?
#[derive(Resource)]
struct ShowCenters(bool);

/// Are we showing the borders of the plates?
#[derive(Resource)]
struct ShowBorders(bool);

#[derive(Debug, Default, Clone, Copy, Resource, PartialEq)]
enum ColorKind {
    Plates,
    #[default]
    Height,
    Density,
    Features,
}

#[derive(Resource)]
struct TerrainData(TectonicState, Box<[LinearRgba]>);

#[derive(Resource)]
struct TectonicScale(f32);

#[derive(Resource, Default, Clone, Serialize, Deserialize)]
struct NoiseSourceRes {
    depth: u8,
    #[serde(rename = "layer")]
    layers: Vec<NoiseSourceBuilder>,
}

#[derive(Resource)]
struct NoiseTerrain(Vec<(Shifted<ValueOrGradient>, f32)>);

#[derive(Resource)]
struct DockedControls {
    display: bool,
    oceans: bool,
    noise: bool,
    tectonics: bool,
    editor: bool,
}

#[derive(Resource)]
struct DockedMap(bool);

#[derive(Resource)]
struct TerrainSteps(u16);

#[derive(Resource)]
struct TimeScale(f32);

#[derive(Debug, Default, Clone, Copy, PartialEq, Resource)]
enum CameraFocus {
    #[default]
    Planet,
    Star,
}

#[derive(Clone, Copy, Resource, Serialize, Deserialize)]
struct OrbitParams {
    distance: f32,
    year_length: f32,
    day_length: f32,
    axial_tilt: f32,
}

#[derive(Component)]
struct Planet;

#[derive(Component)]
struct MiniMap;

#[derive(Component)]
struct PlanetCenter;

#[derive(Event)]
struct ReloadTerrain;

#[derive(Event)]
struct RecolorPlates;

const WIDTH: usize = 600;
const HEIGHT: usize = 300;
const VIEW_WIDTH: u32 = 400;
const VIEW_HEIGHT: u32 = 200;

fn linear(w: f32) -> f32 {
    w
}

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
            scale: linear,
        })
    } else {
        ValueOrGradient::Value(ValueCellNoise {
            depth,
            hasher: thread_rng()
                .sample_iter(rand_distr::Standard)
                .take(factor::healpix::n_hash(depth) as _)
                .collect::<Box<[f32]>>(),
            scale: linear,
        })
    };
    Shifted::new(base, shift)
}

#[inline]
const fn true_abool() -> AtomicBool {
    AtomicBool::new(true)
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct NoiseSourceBuilder {
    gradient: bool,
    depth: u8,
    shift: f32,
    scale: f32,
    #[serde(skip, default = "true_abool")]
    changed: AtomicBool,
}
impl NoiseSourceBuilder {
    #[allow(dead_code)]
    pub const fn value(depth: u8, shift: f32, scale: f32) -> Self {
        Self {
            gradient: false,
            depth,
            shift,
            scale,
            changed: AtomicBool::new(true),
        }
    }
    #[allow(dead_code)]
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

#[derive(Serialize, Deserialize)]
struct OceanConfig {
    depth: f32,
    show: bool,
}

#[derive(Serialize, Deserialize)]
struct TectonicConfig {
    scale: f32,
    depth: u8,
    steps: u16,
}

#[derive(Serialize)]
struct ConfigSerShim<'a> {
    oceans: OceanConfig,
    tectonic: TectonicConfig,
    orbit: OrbitParams,
    noise: &'a NoiseSourceRes,
}
#[derive(Deserialize)]
struct ConfigDeShim {
    oceans: OceanConfig,
    tectonic: TectonicConfig,
    orbit: OrbitParams,
    noise: NoiseSourceRes,
}
struct RandomColor;
impl Distribution<LinearRgba> for RandomColor {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> LinearRgba {
        LinearRgba::rgb(rng.gen(), rng.gen(), rng.gen())
    }
}

fn make_noise<'a, I: IntoIterator<Item = &'a NoiseSourceBuilder>>(
    iter: I,
) -> Vec<(Shifted<ValueOrGradient>, f32)> {
    iter.into_iter().map(NoiseSourceBuilder::build).collect()
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                fit_canvas_to_parent: true,
                title: "Terrain".into(),
                ..default()
            }),
            ..default()
        }))
        .add_plugins(EguiPlugin)
        .init_state::<AppState>()
        .add_sub_state::<Simulating>()
        .add_event::<ReloadTerrain>()
        .add_event::<RecolorPlates>()
        .insert_resource(TimeScale(1.0))
        .insert_resource(TectonicDepth(5))
        .insert_resource(TectonicScale(1.0))
        .insert_resource(ShowBorders(false))
        .insert_resource(ShowCenters(false))
        .insert_resource(DockedControls {
            display: true,
            oceans: true,
            noise: true,
            tectonics: true,
            editor: true,
        })
        .insert_resource(DockedMap(true))
        .insert_resource(TerrainSteps(0))
        .insert_resource(LayerFilter::All)
        .insert_resource(NoiseSourceRes::default())
        .insert_resource(ColorKind::Height)
        .insert_resource(CameraFocus::Planet)
        .insert_resource(ShowOceans {
            show: false,
            depth: 0.5,
        })
        .insert_resource(OrbitParams {
            distance: 200.0,
            year_length: 120.0,
            day_length: 2.0,
            axial_tilt: 0.0,
        })
        .insert_resource(Time::<Fixed>::from_hz(2.0))
        .add_systems(Startup, setup)
        .add_systems(PostStartup, (reload_noise, setup_tectonics))
        .add_systems(
            PreUpdate,
            setup_tectonics.run_if(resource_changed::<TectonicDepth>),
        )
        .add_systems(
            Update,
            (
                handle_keypresses,
                update_ui.after(setup),
                update_terrain_min,
                update_positions,
                update_texture.run_if(
                    resource_changed::<NoiseTerrain>
                        .or_else(resource_changed::<TerrainData>)
                        .or_else(resource_changed::<ShowOceans>)
                        .or_else(resource_changed::<LayerFilter>)
                        .or_else(resource_changed::<ColorKind>)
                        .or_else(resource_changed::<ShowBorders>)
                        .or_else(resource_changed::<ShowCenters>),
                ),
                update_noise_terrain
                    .after(reload_noise)
                    .run_if(resource_exists_and_changed::<NoiseSourceRes>),
                reload_noise.run_if(on_event::<ReloadTerrain>()),
                reparent_camera.run_if(resource_changed::<CameraFocus>),
            ),
        )
        .add_systems(FixedUpdate, update_terrain.run_if(in_state(Simulating)))
        .add_systems(
            OnEnter(AppState::Tectonics {
                iter: 0,
                running: false,
            }),
            setup_tectonics,
        )
        .run();
}

fn setup(
    mut commands: Commands,
    mut contexts: EguiContexts,
    mut meshes: ResMut<Assets<Mesh>>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    window: Query<Entity, With<PrimaryWindow>>,
    params: Res<OrbitParams>,
) {
    contexts
        .ctx_for_entity_mut(window.single())
        .data_mut(|mem| {
            let data = mem
                .get_persisted_mut_or_insert_with(egui::Id::new("noise-layers"), || {
                    NoiseSourceRes {
                        depth: 6,
                        layers: vec![
                            NoiseSourceBuilder::value(1, 0.0, 0.15),
                            NoiseSourceBuilder::value(1, 0.1, 0.15),
                            NoiseSourceBuilder::value(2, 0.2, 0.294),
                            NoiseSourceBuilder::value(2, 0.3, 0.294),
                            NoiseSourceBuilder::value(3, 0.5, 0.05),
                            NoiseSourceBuilder::value(3, 0.7, 0.05),
                            NoiseSourceBuilder::gradient(5, 0.0, 0.01),
                        ],
                    }
                })
                .clone();
            commands.insert_resource(data);
        });

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

    commands
        .spawn((
            TransformBundle {
                local: Transform::from_xyz(params.distance, 0.0, 0.0),
                ..default()
            },
            PlanetCenter,
        ))
        .with_children(|commands| {
            commands.spawn((
                PbrBundle {
                    mesh: meshes.add(Sphere::new(5.0).mesh().uv(32, 18)),
                    material: materials.add(StandardMaterial {
                        base_color_texture: Some(image.clone()),
                        ..default()
                    }),
                    transform: Transform::from_xyz(0.0, 0.0, 0.0),
                    // .with_rotation(Quat::from_rotation_x(-FRAC_PI_2)),
                    ..default()
                },
                Planet,
            ));
            commands.spawn(Camera3dBundle {
                transform: Transform::from_xyz(50.0, 20.0, 10.0)
                    .looking_at(Vec3::new(0.0, 0.0, 0.0), Vec3::Z),
                ..default()
            });
        });

    let radius = 50.0;
    commands
        .spawn(PbrBundle {
            mesh: meshes.add(Sphere::new(radius).mesh().ico(10).unwrap()),
            material: materials.add(StandardMaterial {
                unlit: true,
                ..default()
            }),
            transform: Transform::from_xyz(0.0, 0.0, 0.0)
                .with_rotation(Quat::from_rotation_x(-FRAC_PI_2)),
            ..default()
        })
        .with_children(|commands| {
            commands.spawn(PointLightBundle {
                point_light: PointLight {
                    shadows_enabled: true,
                    intensity: 500000000.,
                    range: 1000.0,
                    shadow_depth_bias: 0.2,
                    radius,
                    ..default()
                },
                transform: Transform::from_xyz(0.0, 0.0, 0.0),
                ..default()
            });
        });

    commands.spawn((image, MiniMap));
}

fn handle_keypresses(
    mut contexts: EguiContexts,
    keys: Res<ButtonInput<KeyCode>>,
    clicks: Res<ButtonInput<MouseButton>>,
    mut drags: EventReader<MouseMotion>,
    mut scroll: EventReader<MouseWheel>,
    mut camera_transform: Query<&mut Transform, With<Camera3d>>,
    state: Res<State<AppState>>,
    mut next_state: ResMut<NextState<AppState>>,
    mut oceans: ResMut<ShowOceans>,
    mut centers: ResMut<ShowCenters>,
    mut recolor_evt: EventWriter<RecolorPlates>,
    mut exit_evt: EventWriter<AppExit>,
) {
    let has_focus = {
        let ctx = contexts.ctx_mut();
        ctx.is_pointer_over_area() || ctx.memory(|mem| mem.focused().is_some())
    };
    if keys.any_pressed([KeyCode::ControlLeft, KeyCode::ControlRight])
        && keys.just_pressed(KeyCode::KeyW)
    {
        exit_evt.send(AppExit::Success);
    }
    if !has_focus {
        if keys.just_pressed(KeyCode::KeyO) {
            oceans.show = !oceans.show;
        }
        let mut camera = camera_transform.single_mut();
        if clicks.pressed(MouseButton::Left) {
            for &MouseMotion {
                delta: Vec2 { x, y },
            } in drags.read()
            {
                let rot = Quat::from_euler(EulerRot::YXZ, x * -0.1, y * -0.1, 0.0);
                camera.rotation *= rot;
                camera.translation = rot.mul_vec3(camera.translation);
            }
            *camera = camera.looking_at(Vec3::ZERO, Vec3::Y);
        }
        for &MouseWheel { y, .. } in scroll.read() {
            camera.translation *= 0.99f32.powf(y);
        }
    }
    match *state.get() {
        AppState::Tectonics { iter, running } => {
            if keys.just_pressed(KeyCode::Space) {
                next_state.set(AppState::Tectonics {
                    iter,
                    running: !running,
                });
            }
            if !has_focus {
                if keys.just_pressed(KeyCode::KeyC) {
                    recolor_evt.send(RecolorPlates);
                }
                if keys.just_pressed(KeyCode::KeyX) {
                    centers.0 = !centers.0;
                }
            }
        }
    }
}

fn update_ui(
    mut contexts: EguiContexts,
    mut coloring: ResMut<ColorKind>,
    mut docked_controls: ResMut<DockedControls>,
    mut docked_map: ResMut<DockedMap>,
    filter: ResMut<LayerFilter>,
    mut noise: ResMut<NoiseSourceRes>,
    mut terrain: ResMut<NoiseTerrain>,
    mut tect_depth: ResMut<TectonicDepth>,
    mut orbit_params: ResMut<OrbitParams>,
    (mut terr_scale, mut tect_steps): (ResMut<TectonicScale>, ResMut<TerrainSteps>),
    (mut borders, mut centers, mut oceans, mut focus): (
        ResMut<ShowBorders>,
        ResMut<ShowCenters>,
        ResMut<ShowOceans>,
        ResMut<CameraFocus>,
    ),
    (state, mut next_state): (Res<State<AppState>>, ResMut<NextState<AppState>>),
    (mut reroll_rand, mut recolor_plates): (EventWriter<ReloadTerrain>, EventWriter<RecolorPlates>),
    (mut wip_layer, mut code_editing, old_filter, mut file_load): (
        Local<Option<NoiseSourceBuilder>>,
        Local<Option<String>>,
        Local<LayerFilter>,
        Local<Arc<OnceLock<String>>>,
    ),
    (primary, minimap): (
        Query<Entity, With<PrimaryWindow>>,
        Query<&Handle<Image>, With<MiniMap>>,
    ),
) {
    if let Some(input) = file_load.get() {
        *code_editing = None;
        match toml::from_str(input) {
            Ok(ConfigDeShim {
                oceans:
                    OceanConfig {
                        depth: ocean_depth,
                        show,
                    },
                tectonic:
                    TectonicConfig {
                        scale,
                        depth: tdepth,
                        steps,
                    },
                orbit,
                noise: new_noise,
            }) => {
                oceans.depth = ocean_depth;
                oceans.show = show;
                terr_scale.0 = scale;
                *noise = new_noise;
                tect_depth.0 = tdepth;
                tect_steps.0 = steps;
                *orbit_params = orbit;
            }
            Err(_err) => {
                // TODO: handle this
            }
        }
        *file_load = Arc::default();
    }
    let filter = UnsafeCell::new(filter);
    let old_filter = UnsafeCell::new(old_filter);
    let noise = UnsafeCell::new(noise);
    let oceans = UnsafeCell::new(oceans);
    let terr_scale = UnsafeCell::new(terr_scale);
    let depth = UnsafeCell::new(tect_depth);
    let tect_steps = UnsafeCell::new(tect_steps);
    let orbit_params = UnsafeCell::new(orbit_params);
    let DockedControls {
        display: dock_display,
        oceans: dock_oceans,
        noise: dock_noise,
        tectonics: dock_tectonics,
        editor: dock_editor,
    } = &mut *docked_controls;
    let map_docked = docked_map.0;
    let image = contexts.add_image(minimap.single().clone());
    let context = contexts.ctx_for_entity_mut(primary.single());
    let render_display = {
        let docked = *dock_display;
        let render = |ui: &mut egui::Ui| {
            let label = if *dock_display { "Undock" } else { "Dock" };
            if ui.button(label).clicked() {
                *dock_display = !*dock_display;
            }
            if ui
                .checkbox(&mut borders.bypass_change_detection().0, "Plate Boundaries")
                .changed()
            {
                let _ = &mut *borders;
            }
            if ui
                .checkbox(&mut centers.bypass_change_detection().0, "Plate Centers")
                .changed()
            {
                let _ = &mut *centers;
            }
            let old = *coloring;
            egui::ComboBox::new("color-kind", "Coloring")
                .selected_text(format!("{old:?}"))
                .show_ui(ui, |ui| {
                    let r = coloring.bypass_change_detection();
                    ui.selectable_value(r, ColorKind::Plates, "Plates");
                    ui.selectable_value(r, ColorKind::Features, "Features");
                    ui.selectable_value(r, ColorKind::Height, "Height");
                    ui.selectable_value(r, ColorKind::Density, "Density");
                });
            if *coloring != old {
                let _ = &mut *coloring;
            }
            let old = *focus;
            egui::ComboBox::new("camera-focus", "Camera Focus")
                .selected_text(format!("{old:?}"))
                .show_ui(ui, |ui| {
                    let r = focus.bypass_change_detection();
                    ui.selectable_value(r, CameraFocus::Planet, "Planet");
                    ui.selectable_value(r, CameraFocus::Star, "Star");
                });
            if *focus != old {
                let _ = &mut *focus;
            }
            if ui.button("Recolor Plates").clicked() {
                recolor_plates.send(RecolorPlates);
            }
        };
        if docked {
            Some(|ui: &mut egui::Ui| {
                ui.collapsing(egui::RichText::new("Display").size(18.0), render);
            })
        } else {
            egui::Window::new("Display").show(context, render);
            None
        }
    };
    let render_oceans = {
        let docked = *dock_oceans;
        let render = |ui: &mut egui::Ui| {
            let oceans = unsafe { &mut *oceans.get() };
            let label = if *dock_oceans { "Undock" } else { "Dock" };
            if ui.button(label).clicked() {
                *dock_oceans = !*dock_oceans;
            }
            if ui
                .checkbox(&mut oceans.bypass_change_detection().show, "Show Oceans")
                .changed()
            {
                let _ = &mut **oceans;
            }
            if ui
                .add(egui::Slider::new(
                    &mut oceans.bypass_change_detection().depth,
                    0.0..=1.0,
                ))
                .changed()
            {
                let _ = &mut **oceans;
            }
        };
        if docked {
            Some(|ui: &mut egui::Ui| {
                ui.collapsing(egui::RichText::new("Oceans").size(18.0), render);
            })
        } else {
            egui::Window::new("Oceans").show(context, render);
            None
        }
    };
    let render_noise = {
        // safety: this doesn't escape, and only one of these functions runs at once
        let (filter, old_filter, noise) = unsafe {
            (
                &mut *filter.get(),
                &mut *old_filter.get(),
                &mut *noise.get(),
            )
        };
        let docked = *dock_noise;
        let render = |ui: &mut egui::Ui| {
            let label = if *dock_noise { "Undock" } else { "Dock" };
            if ui.button(label).clicked() {
                *dock_noise = !*dock_noise;
            }
            if ui.button("Reload All").clicked() {
                reroll_rand.send(ReloadTerrain);
            }
            if ui
                .add(
                    egui::Slider::new(&mut noise.bypass_change_detection().depth, 2..=7)
                        .text("Sample Depth"),
                )
                .changed()
            {
                let _ = &mut **noise;
            }
            let mut showing = None;
            egui::ScrollArea::vertical().show(ui, |ui| {
                let mut changed = false;
                let mut delete = None;
                let mut normalize = None;
                {
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
                                    noise.layers.push(wip_layer.take().unwrap());
                                    changed = true;
                                    ui.memory_mut(|mem| mem.close_popup());
                                }
                                if ui.button("Cancel").clicked() {
                                    *wip_layer = None;
                                    ui.memory_mut(|mem| mem.close_popup());
                                }
                            })
                        },
                    );
                }
                let sum = noise.layers.iter().map(|l| l.scale).sum::<f32>();
                let mut new_sum = sum;
                ui.add_enabled(
                    sum != 0.0,
                    egui::Slider::new(&mut new_sum, 0.0..=2.0).text("Scale"),
                );
                if sum != new_sum {
                    let scale = new_sum / sum;
                    for (l, t) in noise.layers.iter_mut().zip(&mut terrain.0) {
                        let val = (l.scale * scale * 1000.0).round() * 0.001;
                        l.scale = val;
                        t.1 = val;
                    }
                }

                for (n, layer) in noise
                    .bypass_change_detection()
                    .layers
                    .iter_mut()
                    .enumerate()
                {
                    let frame = egui::Frame::group(ui.style()).show(ui, |ui| {
                        ui.set_min_width(ui.max_rect().width());
                        ui.label(format!(
                            "Layer: {}\nScale: {:.3}\nShift: {:.3}\nGradient: {}",
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
                                .add(egui::Slider::new(&mut layer.shift, 0.0..=3.0).text("Shift"))
                                .changed()
                            {
                                changed = true; // don't count this as a change because it's lazy
                                terrain.0[n].0.shift = layer.shift;
                            }
                            *layer.changed.get_mut() |=
                                ui.checkbox(&mut layer.gradient, "Gradient").changed();
                            if ui.button("Normalize Others").clicked() {
                                normalize = Some(n);
                            }
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
                match (**filter, showing) {
                    (LayerFilter::All | LayerFilter::AllNoise, None) => {}
                    (LayerFilter::NoiseLayer(a), Some(b)) if a == b => {}
                    (_, None) => **filter = LayerFilter::All,
                    (_, Some(n)) => **filter = LayerFilter::NoiseLayer(n),
                }
                if let Some(idx) = delete {
                    noise.layers.remove(idx);
                    terrain.0.remove(idx);
                    changed = true;
                } else if let Some(idx) = normalize {
                    let d0 = 1.0 - noise.layers[idx].scale;
                    let mut sum = 0.0;
                    for (n, l) in noise.layers.iter().enumerate() {
                        if n == idx {
                            continue;
                        }
                        sum += l.scale;
                    }
                    if sum != 0.0 {
                        let scale = d0 / sum;
                        for (n, (l, t)) in noise.layers.iter_mut().zip(&mut terrain.0).enumerate() {
                            if n == idx {
                                continue;
                            }
                            let val = (l.scale * scale * 1000.0).round() * 0.001;
                            l.scale = val;
                            t.1 = val;
                        }
                    }
                }
                if changed {
                    let _ = &mut **noise;
                }
            });
            if showing.is_none() {
                if ui.ui_contains_pointer() {
                    if !matches!(**filter, LayerFilter::AllNoise | LayerFilter::NoiseLayer(_)) {
                        **old_filter = **filter;
                        **filter = LayerFilter::AllNoise;
                    }
                } else if **filter == LayerFilter::AllNoise {
                    **filter = **old_filter;
                }
            }
        };
        if docked {
            Some(|ui: &mut egui::Ui| {
                ui.collapsing(egui::RichText::new("Noise").size(18.0), render);
            })
        } else {
            egui::Window::new("Noise")
                .default_width(200.0)
                .show(context, render);
            None
        }
    };
    let render_tectonics = {
        let docked = *dock_tectonics;
        let render = |ui: &mut egui::Ui| {
            // safety: this doesn't escape, and only one of these functions runs at once
            let (filter, old_filter, terr_scale, depth, tect_steps) = unsafe {
                (
                    &mut *filter.get(),
                    &mut *old_filter.get(),
                    &mut *terr_scale.get(),
                    &mut *depth.get(),
                    &mut *tect_steps.get(),
                )
            };
            let label = if *dock_tectonics { "Undock" } else { "Dock" };
            if ui.button(label).clicked() {
                *dock_tectonics = !*dock_tectonics;
            }
            match **state {
                AppState::Tectonics { iter, mut running } => {
                    ui.label(format!("Simulating\nStep: {iter}"));
                    ui.horizontal(|ui| {
                        if ui.checkbox(&mut running, "Running").changed() {
                            next_state.set(AppState::Tectonics { iter, running });
                        }
                        if ui.button("Reset").clicked() {
                            let _ = &mut **depth;
                        }
                    });
                }
            }

            if ui
                .add(
                    egui::Slider::new(&mut tect_steps.bypass_change_detection().0, 0..=100)
                        .text("Min. Steps"),
                )
                .changed()
            {
                let _ = &mut **tect_steps;
            }

            if ui
                .add(egui::Slider::new(&mut depth.bypass_change_detection().0, 3..=6).text("Depth"))
                .changed()
            {
                let _ = &mut **depth;
            }

            if ui
                .add(
                    egui::Slider::new(&mut terr_scale.bypass_change_detection().0, 0.0..=3.0)
                        .text("Scale"),
                )
                .changed()
            {
                let _ = &mut **terr_scale;
            }
            if ui.ui_contains_pointer() {
                if **filter != LayerFilter::Tectonics {
                    **old_filter = **filter;
                    **filter = LayerFilter::Tectonics;
                }
            } else if **filter == LayerFilter::Tectonics {
                **filter = **old_filter;
            }
        };
        if docked {
            Some(|ui: &mut egui::Ui| {
                ui.collapsing(egui::RichText::new("Tectonics").size(18.0), render);
            })
        } else {
            egui::Window::new("Tectonics").show(context, render);
            None
        }
    };
    let render_editor = {
        let docked = *dock_editor;
        let render = |ui: &mut egui::Ui| {
            let (noise, oceans, terr_scale, depth, tect_steps, orbit_params) = unsafe {
                (
                    &mut *noise.get(),
                    &mut *oceans.get(),
                    &mut *terr_scale.get(),
                    &mut *depth.get(),
                    &mut *tect_steps.get(),
                    &mut *orbit_params.get(),
                )
            };

            ui.horizontal(|ui| {
                if ui.button("Apply").clicked() {
                    match toml::from_str(&code_editing.take().unwrap()) {
                        Ok(ConfigDeShim {
                            oceans:
                                OceanConfig {
                                    depth: ocean_depth,
                                    show,
                                },
                            tectonic:
                                TectonicConfig {
                                    scale,
                                    depth: tect_depth,
                                    steps,
                                },
                            orbit,
                            noise: new_noise,
                        }) => {
                            oceans.depth = ocean_depth;
                            oceans.show = show;
                            terr_scale.0 = scale;
                            depth.0 = tect_depth;
                            tect_steps.0 = steps;
                            **noise = new_noise;
                            **orbit_params = orbit;
                        }
                        Err(err) => {
                            ui.colored_label(ui.style().visuals.error_fg_color, err.to_string());
                        }
                    }
                }
                if ui.button("Reset").clicked() {
                    *code_editing = None;
                }
                ui.with_layout(egui::Layout::right_to_left(egui::Align::Min), |ui| {
                    let label = if *dock_editor { "Undock" } else { "Dock" };
                    if ui.button(label).clicked() {
                        *dock_editor = !*dock_editor;
                    }
                })
            });
            ui.horizontal(|ui| {
                if ui.button("Save").clicked() {
                    let save_code = code_editing.clone().unwrap_or_default();
                    bevy::tasks::IoTaskPool::get()
                        .spawn(async move {
                            let file = rfd::AsyncFileDialog::new()
                                .add_filter("TOML", &["toml"])
                                .save_file()
                                .await;
                            if let Some(f) = file {
                                let _ = f.write(save_code.as_bytes()).await;
                            }
                        })
                        .detach();
                }
                if ui.button("Load").clicked() {
                    let handle = Arc::clone(&file_load);
                    bevy::tasks::IoTaskPool::get()
                        .spawn(async move {
                            let file = rfd::AsyncFileDialog::new()
                                .add_filter("TOML", &["toml"])
                                .pick_file()
                                .await;
                            if let Some(f) = file {
                                let data = f.read().await;
                                let input = String::from_utf8(data).unwrap_or_default(); // TODO: handle non-UTF8 input
                                let _ = handle.set(input);
                            }
                        })
                        .detach();
                }
            });
            if noise.is_changed()
                || oceans.is_changed()
                || terr_scale.is_changed()
                || depth.is_changed()
                || tect_steps.is_changed()
            {
                *code_editing = None;
            }
            egui::ScrollArea::both().show(ui, |ui| {
                ui.code_editor(code_editing.get_or_insert_with(|| {
                    toml::to_string_pretty(&ConfigSerShim {
                        oceans: OceanConfig {
                            depth: oceans.depth,
                            show: oceans.show,
                        },
                        tectonic: TectonicConfig {
                            scale: terr_scale.0,
                            depth: depth.0,
                            steps: tect_steps.0,
                        },
                        orbit: **orbit_params,
                        noise,
                    })
                    .unwrap()
                }));
            });
        };
        if docked {
            Some(|ui: &mut egui::Ui| {
                ui.collapsing(egui::RichText::new("Editor").size(18.0), render);
            })
        } else {
            egui::Window::new("Editor")
                .default_width(200.0)
                .show(context, render);
            None
        }
    };
    if render_display.is_some()
        || render_oceans.is_some()
        || render_noise.is_some()
        || render_tectonics.is_some()
        || render_editor.is_some()
    {
        let undock = egui::SidePanel::left("Controls")
            .min_width(180.0)
            .show(context, |ui| {
                let undock = ui.button("Undock All").clicked();
                if let Some(render) = render_display {
                    render(ui);
                }
                if let Some(render) = render_oceans {
                    render(ui);
                }
                if let Some(render) = render_noise {
                    render(ui);
                }
                if let Some(render) = render_tectonics {
                    render(ui);
                }
                if let Some(render) = render_editor {
                    render(ui);
                }
                undock
            })
            .inner;
        if undock {
            *docked_controls = DockedControls {
                display: false,
                oceans: false,
                noise: false,
                tectonics: false,
                editor: false,
            };
        }
    }
    let render_map = |ui: &mut egui::Ui| {
        let label = if map_docked { "Undock" } else { "Dock" };
        if ui.button(label).clicked() {
            docked_map.0 = !docked_map.0;
        }
        ui.image(egui::load::SizedTexture::new(
            image,
            (VIEW_WIDTH as f32, VIEW_HEIGHT as f32),
        ));
    };
    let mut window = egui::Window::new("World Map")
        .resizable(false)
        .default_pos(context.screen_rect().max - egui::vec2(VIEW_WIDTH as _, VIEW_HEIGHT as _));
    if map_docked {
        window = window.anchor(egui::Align2::RIGHT_BOTTOM, egui::Vec2::ZERO);
    }
    window.show(context, render_map);
}

fn setup_tectonics(
    mut commands: Commands,
    mut next_state: ResMut<NextState<AppState>>,
    depth: Res<TectonicDepth>,
) {
    let image_map = (0..(WIDTH * HEIGHT))
        .map(|i| {
            use std::f64::consts::*;
            let x = ((i % WIDTH) as f64).mul_add(TAU / WIDTH as f64, -PI);
            let y = ((i / WIDTH) as f64).mul_add(-PI / HEIGHT as f64, FRAC_PI_2);
            cdshealpix::nested::hash(depth.0, x, y) as usize
        })
        .collect();
    let state = init_terrain(depth.0, &mut thread_rng());
    let colors: Box<[LinearRgba]> = thread_rng()
        .sample_iter(RandomColor)
        .take(state.plates().len())
        .collect();
    commands.insert_resource(TerrainData(state, colors));
    commands.insert_resource(TectonicMap(image_map));
    next_state.set(AppState::Tectonics {
        iter: 0,
        running: false,
    });
}

fn update_terrain(
    mut terr: ResMut<TerrainData>,
    state: Res<State<AppState>>,
    mut next_state: ResMut<NextState<AppState>>,
) {
    step_terrain(&mut terr.0, &mut thread_rng());
    let AppState::Tectonics { running, iter } = **state;
    next_state.set(AppState::Tectonics {
        running,
        iter: iter + 1,
    });
}

fn update_terrain_min(
    steps: Res<TerrainSteps>,
    mut terr: ResMut<TerrainData>,
    state: Res<State<AppState>>,
    mut next_state: ResMut<NextState<AppState>>,
) {
    let AppState::Tectonics { running, mut iter } = **state;
    if iter < steps.0 {
        let start = Instant::now();
        while iter < steps.0 {
            step_terrain(&mut terr.0, &mut thread_rng());
            iter += 1;
            if start.elapsed() > Duration::from_millis(100) {
                break;
            }
        }
        next_state.set(AppState::Tectonics { iter, running });
    }
}

fn reload_noise(mut commands: Commands, noise: Res<NoiseSourceRes>) {
    commands.insert_resource(NoiseTerrain(make_noise(noise.layers.iter())));
    commands.insert_resource(NoiseHeights(
        vec![0.0; 12 * (1 << (2 * noise.depth))].into_boxed_slice(),
    ));
    let image_map = (0..(WIDTH * HEIGHT))
        .map(|i| {
            use std::f64::consts::*;
            let x = ((i % WIDTH) as f64).mul_add(TAU / WIDTH as f64, -PI);
            let y = ((i / WIDTH) as f64).mul_add(-PI / HEIGHT as f64, FRAC_PI_2);
            cdshealpix::nested::hash(noise.depth, x, y) as usize
        })
        .collect();
    commands.insert_resource(NoiseMap(image_map));
}

fn update_noise_terrain(
    mut commands: Commands,
    mut contexts: EguiContexts,
    noise: Res<NoiseSourceRes>,
    mut terr: ResMut<NoiseTerrain>,
    mut samples: ResMut<NoiseHeights>,
    mut last_depth: Local<u8>,
) {
    // let mut changed = false;
    for (b, n) in noise
        .layers
        .iter()
        .zip(&mut terr.bypass_change_detection().0)
    {
        if b.changed.load(Ordering::Relaxed) {
            *n = b.build();
            // changed = true;
        }
    }
    if noise.layers.len() > terr.0.len() {
        let start = terr.0.len();
        terr.0
            .extend(noise.layers[start..].iter().map(NoiseSourceBuilder::build));
    }
    let layer = factor::healpix::nested::get(noise.depth);
    for i in 0..layer.n_hash() {
        let (lon, lat) = layer.center(i);
        let height = terr.0.get_height(lon as _, lat as _);
        samples.0[i as usize] = height;
    }
    let _ = &mut *terr;
    if noise.depth != *last_depth {
        *last_depth = noise.depth;
        let image_map = (0..(WIDTH * HEIGHT))
            .map(|i| {
                use std::f64::consts::*;
                let x = ((i % WIDTH) as f64).mul_add(TAU / WIDTH as f64, -PI);
                let y = ((i / WIDTH) as f64).mul_add(-PI / HEIGHT as f64, FRAC_PI_2);
                layer.hash(x, y) as usize
            })
            .collect();
        commands.insert_resource(NoiseMap(image_map));
    }
    contexts.ctx_mut().data_mut(|mem| {
        mem.insert_persisted(egui::Id::new("noise-layers"), NoiseSourceRes::clone(&noise));
    })
}

fn update_texture(
    colors: Res<ColorKind>,
    noise: (Res<NoiseHeights>, Res<NoiseMap>, Res<NoiseTerrain>),
    tect: (Res<TerrainData>, Res<TectonicMap>, Res<TectonicScale>),
    oceans: Res<ShowOceans>,
    filter: Res<LayerFilter>,
    borders: Res<ShowBorders>,
    mut images: ResMut<Assets<Image>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut planet: Query<&mut Handle<StandardMaterial>, With<Planet>>,
    mut minimap: Query<&mut Handle<Image>, With<MiniMap>>,
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
    'pixels: for (n, d) in bytemuck::cast_slice_mut(&mut img.data)
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
                    let sqdist = (cx - plate.center_long).powi(2) + (cy - plate.center_lat).powi(2);
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
        use std::f32::consts::*;
        let cell_idx = tect.1 .0[n];
        let cell = tect.0 .0.cells()[cell_idx];
        if borders.0 && tect.0 .0.boundaries().contains(cell_idx as _) {
            *d = LinearRgba::rgb(1.0, 0.0, 1.0).as_u32();
            continue;
        }
        let x = ((n % WIDTH) as f32) * TAU / WIDTH as f32;
        let y = ((n / WIDTH) as f32).mul_add(-PI / HEIGHT as f32, FRAC_PI_2);
        let height = match *filter {
            LayerFilter::All => {
                noise.0 .0[noise.1 .0[n]]
                    + cell.height.mul_add(0.1, 0.2).clamp(0.0, 1.0) * tect.2 .0
            }
            LayerFilter::Tectonics => cell.height.mul_add(0.1, 0.2).clamp(0.0, 1.0),
            LayerFilter::AllNoise => noise.0 .0[noise.1 .0[n]],
            LayerFilter::NoiseLayer(idx) => noise.2 .0[idx].get_height(x, y),
        };
        *d = match *colors {
            ColorKind::Plates => tect.0 .1[cell.plate as usize],
            ColorKind::Features => {
                let base = match cell.feats.kind {
                    CellFeatureKind::None => {
                        if tect.0 .0.boundaries().contains(cell_idx as _) {
                            LinearRgba::rgb(1.0, 1.0, 0.0)
                        } else {
                            LinearRgba::BLACK
                        }
                    }
                    CellFeatureKind::Subduction => LinearRgba::BLUE,
                    CellFeatureKind::Mountain => LinearRgba::GREEN,
                    CellFeatureKind::Ridge => LinearRgba::RED,
                };
                base.with_luminance((-(cell.feats.dist as f32 * 0.5).powi(2)).exp())
            }
            c => {
                if *filter == LayerFilter::All && oceans.show && height < oceans.depth {
                    LinearRgba::from(Srgba::rgb_u8(0, 51, 102)).with_luminance(height * 0.5)
                } else if c == ColorKind::Density {
                    let dens = (cell.density as f32).mul_add(0.0025, -0.05);
                    let dens = dens.mul_add(0.2, -0.1);
                    let base = LinearRgba::rgb(0.5 - dens, 0.5, 0.5 + dens);
                    base.with_luminance(height)
                } else {
                    LinearRgba::gray(height)
                }
            }
        }
        .as_u32();
    }
    let image = images.add(img);
    *planet.single_mut() = materials.add(StandardMaterial {
        base_color_texture: Some(image.clone()),
        ..default()
    });
    *minimap.single_mut() = image;
}

fn update_positions(
    mut center: Query<&mut Transform, With<PlanetCenter>>,
    mut planet: Query<&mut Transform, (With<Planet>, Without<PlanetCenter>)>,
    time: Res<Time>,
    scale: Res<TimeScale>,
    params: Res<OrbitParams>,
    mut last_tilt: Local<f32>,
) {
    let delta = time.delta_seconds();
    let mut center = center.single_mut();
    let mut angle = center.translation.xy().to_angle();
    let diff = delta / params.year_length * TAU * scale.0;
    angle += diff;
    info!(angle, diff, "rotating");
    center.translation = Vec2::from_angle(angle).extend(0.0) * params.distance;
    let mut planet = planet.single_mut();
    planet.rotate_x(params.axial_tilt - *last_tilt);
    planet.rotate_axis(
        Dir3::new(Vec2::from_angle(params.axial_tilt).extend(0.0).yzx()).unwrap(),
        delta / params.day_length * TAU * scale.0,
    );
    *last_tilt = params.axial_tilt;
}

fn reparent_camera(
    mut commands: Commands,
    camera: Query<Entity, With<Camera3d>>,
    planet: Query<Entity, With<PlanetCenter>>,
    star: Query<Entity, With<PointLight>>,
    focus: Res<CameraFocus>,
) {
    let parent = match *focus {
        CameraFocus::Planet => planet.single(),
        CameraFocus::Star => star.single(),
    };
    commands.entity(camera.single()).set_parent(parent);
}
