#![feature(array_chunks)]
#![allow(clippy::type_complexity)]

use bevy::ecs::system::SystemId;
use bevy::input::mouse::MouseMotion;
use bevy::math::{Affine2, DVec2};
use bevy::pbr::wireframe::{WireframeConfig, WireframePlugin};
use bevy::prelude::*;
use bevy::render::render_asset::RenderAssetUsages;
use bevy::render::render_resource::{
    Extent3d, TextureDescriptor, TextureDimension, TextureFormat, TextureUsages,
};
use bevy::render::view::RenderLayers;
use bevy::window::CursorGrabMode;
use bevy::window::WindowFocused;
use bevy_egui::{egui, EguiContexts, EguiPlugin};
use factor_common::coords::*;
use factor_common::healpix;
use factor_common::healpix::cds::compass_point::MainWind;
// use rand::prelude::*;
use std::convert::Infallible;
use std::f64::consts::{FRAC_PI_2, TAU};
use std::sync::{LazyLock, OnceLock};
use std::time::Duration;
use triomphe::Arc;

const SCALE: f64 = 1000.0;

const NAN_TRANSFORM: Transform = Transform {
    translation: Vec3::NAN,
    rotation: Quat::NAN,
    scale: Vec3::NAN,
};

// const BASE_TRANSFORM: Transform =
//     Transform::from_rotation(Quat::from_xyzw(0.0, FRAC_1_SQRT_2, 0.0, FRAC_1_SQRT_2));

const BASE_TRANSFORM: Transform = Transform::IDENTITY;

static CACHE: [LazyLock<
    quick_cache::sync::Cache<u64, ([Vec2; 4], Arc<OnceLock<[(u64, Transform); 8]>>)>,
>; 29] = [const { LazyLock::new(|| quick_cache::sync::Cache::new(65536)) }; 29];
fn cache_data(depth: u8, hash: u64) -> ([Vec2; 4], Arc<OnceLock<[(u64, Transform); 8]>>) {
    let Ok(res) = CACHE[depth as usize].get_or_insert_with(&hash, || {
        let layer = healpix::Layer::new(depth);
        let vertices = layer.vertices(hash);
        let center = layer.center(hash);
        let corners = vertices.map(|c2| (get_relative(center, c2) * SCALE).as_vec2()); // I don't know where this 2 came from
        Ok::<_, Infallible>((corners, Arc::new(OnceLock::new())))
    });
    res
}
fn corners_of(depth: u8, hash: u64) -> [Vec2; 4] {
    cache_data(depth, hash).0
}
fn transforms_for(depth: u8, base: u64, neighbor: u64) -> Transform {
    let (verts, slice) = cache_data(depth, base); // verts: S E N W
    let transforms = slice.get_or_init(|| {
        let _guard = info_span!("finding transforms", depth, base).entered();
        let layer = healpix::Layer::new(depth);
        let neighbors = layer.neighbors(base, false);
        let mut has = 0u8;
        let mut out_verts = [[Vec2::NAN; 4]; 8]; // NW NE SE SW N E S W
        let mut out = [(u64::MAX, NAN_TRANSFORM); 8]; // same order
        for (i, wind) in [MainWind::N, MainWind::E, MainWind::S, MainWind::W]
            .into_iter()
            .enumerate()
        {
            if neighbors.get(wind).is_some() {
                has |= 1 << i;
            }
        }
        for (i, wind) in [MainWind::NW, MainWind::NE, MainWind::SE, MainWind::SW]
            .into_iter()
            .enumerate()
        {
            let cell = *neighbors.get(wind).unwrap();
            let _guard = info_span!("ordinal transform", wind = ?[MainWind::NW, MainWind::NE, MainWind::SE, MainWind::SW][i], neighbor = cell).entered();
            let mut new_verts = corners_of(depth, cell); // S E N W
            // new_verts.rotate_left(1);
            let [na, nb, ba, bb] = match i {
                0 => [0, 1, 3, 2],
                1 => [3, 0, 2, 1],
                2 => [2, 3, 1, 0],
                3 => [1, 2, 0, 3],
                _ => unreachable!(),
            };
            let (v1, v2, nv1, nv2) = (verts[ba], verts[bb], new_verts[na], new_verts[nb]);
            let ax1 = nv1 - nv2;
            let ax2 = v1 - v2;
            debug!(?verts, ?new_verts, "available points");
            debug!(%v1, %v2, %nv1, %nv2, "got points");
            debug!(%ax1, %ax2, "created axes");
            let scale = (ax2.length_squared() / ax1.length_squared()).sqrt();
            debug!(scale, "found scale");
            out_verts[i][na] = verts[ba];
            out_verts[i][nb] = verts[bb];
            for v in &mut new_verts {
                *v /= scale;
            }
            let angle = ax2.to_angle() - ax1.to_angle();
            debug!(angle, "found angle");
            let mat2 = Mat2::from_scale_angle(Vec2::splat(scale), angle);
            let transformed = mat2.mul_mat2(&Mat2::from_cols(nv1, nv2));
            let mv1 = transformed.x_axis;
            let translate = v1 - mv1;
            #[cfg(debug_assertions)]
            {
                let mv2 = transformed.y_axis;
                let trans2 = v2 - mv2;
                let len = (translate - trans2).length();
                assert!(
                    len < 0.01,
                    "translations are significantly different: {} vs {}",
                    translate,
                    trans2
                );
            }
            debug!(%translate, "found translation");
            let nc = (na + 2) % 4;
            let nd = (nb + 2) % 4;
            let transformed = mat2.mul_mat2(&Mat2::from_cols(new_verts[nc], new_verts[nd]))
                + Mat2::from_cols(translate, translate);
            out_verts[i][nc] = transformed.x_axis;
            out_verts[i][nd] = transformed.y_axis;
            // out_verts[i].rotate_right(1);
            out[i] = (
                cell,
                Transform {
                    translation: translate.extend(0.0).xzy(),
                    rotation: Quat::from_rotation_y(-angle),
                    scale: Vec3::new(scale, 1.0, scale),
                },
            );
        }
        debug!(verts = ?out_verts[..4], "initial verticces");
        for (i, wind) in [MainWind::N, MainWind::E, MainWind::S, MainWind::W]
            .into_iter()
            .enumerate()
        {
            let Some(&cell) = neighbors.get(wind) else {
                continue;
            };
            let _guard = info_span!("cardinal transform", wind = ?[MainWind::N, MainWind::E, MainWind::S, MainWind::W][i], neighbor = cell).entered();
            let prev = i % 4;
            let next = (i + 1) % 4;
            let new_verts = corners_of(depth, cell); // S E N W
            let [other_v, to_base_v, to_prev_v, to_next_v] = match i {
                0 => [2, 0, 3, 1],
                1 => [1, 3, 2, 0],
                2 => [0, 2, 1, 3],
                3 => [3, 1, 0, 2],
                _ => unreachable!(),
            };
            let to = Mat3::from_cols(
                verts[other_v].extend(1.0),
                out_verts[prev][other_v].extend(1.0),
                out_verts[next][other_v].extend(1.0),
            );
            let from = Mat3::from_cols(
                new_verts[to_base_v].extend(1.0),
                new_verts[to_prev_v].extend(1.0),
                new_verts[to_next_v].extend(1.0),
            );
            debug!(%from, %to, "computing transform");
            let mat3 = to.mul_mat3(&from.inverse());
            #[cfg(debug_assertions)]
            {
                let bottom = mat3.transpose().z_axis;
                assert!((bottom - Vec3::Z).length() < 0.001, "transform isn't affine, bottom row = {bottom}");
            }
            debug!(mat = %mat3, "found transformation");
            let (scale, angle, trans) = Affine2::from_mat3(mat3).to_scale_angle_translation();
            debug!(%scale, angle, %trans, "decomposed matrix");
            out[i + 4] = (
                cell,
                Transform {
                    translation: trans.extend(0.0).xzy(),
                    rotation: Quat::from_rotation_y(-angle),
                    scale: scale.extend(1.0).xzy(),
                },
            );
        }
        out
    });
    transforms
        .iter()
        .find_map(|&(n, t)| (n == neighbor).then_some(t))
        .unwrap_or_else(|| {
            error!(
                depth,
                base, neighbor, "attempted to find transform for non-neighboring cell"
            );
            NAN_TRANSFORM
        })
}

/// Which cell
#[derive(Debug, Clone, Copy, PartialEq, Component)]
struct OwningCell(u64);

#[derive(Debug, Clone, Copy, PartialEq, Component)]
struct CellCenter;

#[derive(Debug, Clone, Copy, Component)]
#[component(storage = "SparseSet")]
struct DespawnTime(Duration);

#[derive(Debug, Clone, Copy, PartialEq, Resource)]
struct LockedMouse(bool);

#[derive(Debug, Clone, Copy, PartialEq, Resource)]
struct MovementSpeed(f32);

#[derive(Debug, Clone, Copy, PartialEq, Resource)]
struct Systems {
    spawn_cell: SystemId<(u64, Entity), ()>,
}

#[derive(Debug, Clone, Copy, Resource)]
struct HealpixParams {
    depth: u8,
    delta: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Resource)]
struct ShowTestPoints(bool);

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                fit_canvas_to_parent: true,
                title: "Surface".into(),
                ..default()
            }),
            ..default()
        }))
        .add_plugins(EguiPlugin)
        .add_plugins(WireframePlugin)
        .insert_resource(HealpixParams { depth: 5, delta: 0 })
        .insert_resource(LockedMouse(false))
        .insert_resource(MovementSpeed(0.1))
        .insert_resource(ShowTestPoints(false))
        .insert_resource(AmbientLight {
            color: Color::WHITE,
            brightness: 100.0,
        })
        .add_systems(Startup, setup)
        .add_systems(FixedUpdate, handle_input)
        .add_systems(PreUpdate, despawn_system)
        .add_systems(
            Update,
            (
                show_ui,
                check_cell,
                load_cells,
                free_mouse,
                update_coords.run_if(resource_changed::<HealpixParams>),
                grab_mouse.run_if(resource_changed::<LockedMouse>),
            ),
        )
        .add_systems(
            PostUpdate,
            (
                nan_checks,
                test_points.run_if(resource_equals(ShowTestPoints(true))),
                render_axes.after(bevy::transform::TransformSystem::TransformPropagate),
            ),
        )
        .run();
}

fn setup(mut commands: Commands) {
    let base = commands.spawn_empty().id();
    commands.spawn((
        Camera3dBundle {
            transform: Transform::from_xyz(1.0, 50.0, -1.0).looking_at(Vec3::ZERO, Dir3::Y),
            ..default()
        },
        RenderLayers::layer(0),
        OwningCell(0),
    ));
    commands.spawn(PointLightBundle {
        point_light: PointLight {
            intensity: 400.0,
            range: 2000.0,
            radius: 10.0,
            shadows_enabled: true,
            ..default()
        },
        transform: Transform::from_xyz(0.0, 30.0, 0.0),
        ..default()
    });
    let spawn_cell = commands.register_one_shot_system(spawn_cell);
    commands.run_system_with_input(spawn_cell, (0, base));
    commands.insert_resource(Systems { spawn_cell });
}

fn handle_input(
    mut locked: ResMut<LockedMouse>,
    mut tests: ResMut<ShowTestPoints>,
    speed: Res<MovementSpeed>,
    keys: Res<ButtonInput<KeyCode>>,
    mut mouse: EventReader<MouseMotion>,
    mut camera: Query<&mut Transform, With<Camera3d>>,
) {
    if keys.just_pressed(KeyCode::Escape) {
        locked.0 = !locked.0;
    }
    if !locked.0 {
        return;
    }
    let mut camera = camera.single_mut();
    let mut velocity = Vec3::ZERO;
    let mut speed = speed.0;
    if keys.pressed(KeyCode::ControlLeft) {
        speed *= 10.0;
    }
    if keys.pressed(KeyCode::Space) {
        velocity += Vec3::Y * speed;
    }
    if camera.translation.y > speed && keys.pressed(KeyCode::ShiftLeft) {
        velocity -= Vec3::Y * speed;
    }
    let forward = camera
        .forward()
        .xz()
        .normalize_or(camera.up().xz())
        .extend(0.0)
        .xzy();
    let right = camera.right();
    if keys.pressed(KeyCode::KeyW) {
        velocity += forward * speed;
    }
    if keys.pressed(KeyCode::KeyA) {
        velocity -= right * speed;
    }
    if keys.pressed(KeyCode::KeyS) {
        velocity -= forward * speed;
    }
    if keys.pressed(KeyCode::KeyD) {
        velocity += right * speed;
    }
    camera.translation += velocity;
    for &MouseMotion {
        delta: delta @ Vec2 { x, y },
    } in mouse.read()
    {
        let before = camera.rotation;
        camera.rotate_y(x * -0.01);
        camera.rotate_local_x(y * -0.01);
        assert!(
            camera.is_finite(),
            "infinite camera transform, before={before}, delta={delta}"
        );
    }
    if keys.pressed(KeyCode::KeyC) {
        tests.0 = !tests.0;
    }
}

fn show_ui(
    mut contexts: EguiContexts,
    time: Res<Time>,
    locked: Res<LockedMouse>,
    mut tests: ResMut<ShowTestPoints>,
    mut speed: ResMut<MovementSpeed>,
    mut params: ResMut<HealpixParams>,
    wireframe: Option<ResMut<WireframeConfig>>,
    mut camera: Query<(&mut Transform, &mut OwningCell), With<Camera3d>>,
    cells: Query<
        (
            &OwningCell,
            &Transform,
            &GlobalTransform,
            Option<&DespawnTime>,
        ),
        (With<CellCenter>, Without<Camera3d>),
    >,
) {
    let ctx = contexts.ctx_mut();
    if locked.0 {
        egui::Area::new(egui::Id::new("HUD"))
            .anchor(egui::Align2::LEFT_TOP, egui::vec2(10.0, 10.0))
            .show(ctx, |ui| {
                let (trans, cell) = camera.single();
                let layer = healpix::Layer::new(params.depth);
                let LonLat { lon, lat } = get_absolute(
                    layer.center(cell.0),
                    trans.translation.xz().as_dvec2() / SCALE,
                );
                let rot = trans.forward();
                ui.label(egui::text::LayoutJob::simple(format!(
                    "Cell: {}\nX: {:.5}, Y: {:.5}, Z: {:.5}\nLon: {lon:.3}, Lat: {lat:.3}\nRotX: {:.3}, RotY: {:.3}\n{} cells loaded",
                    cell.0, trans.translation.x, trans.translation.y, trans.translation.z, rot.xz().to_angle(), rot.y.asin(), cells.iter().count(),
                ), default(), ui.style().visuals.text_color(), 0.0));
            });
    } else {
        let (mut trans, mut containing) = camera.single_mut();
        egui::Window::new("Position").show(ctx, |ui| {
            if ui
                .add(
                    egui::Slider::new(
                        &mut containing.bypass_change_detection().0,
                        0..=healpix::n_hash(params.depth),
                    )
                    .text("Cell"),
                )
                .changed()
            {
                let _ = &mut *containing;
            }
            if ui
                .add(
                    egui::Slider::new(
                        &mut trans.bypass_change_detection().translation.x,
                        -10.0..=10.0,
                    )
                    .text("X")
                    .clamping(egui::SliderClamping::Never),
                )
                .changed()
            {
                let _ = &mut *trans;
            }
            if ui
                .add(
                    egui::Slider::new(
                        &mut trans.bypass_change_detection().translation.y,
                        0.0..=100.0,
                    )
                    .text("Y")
                    .clamping(egui::SliderClamping::Never)
                    .logarithmic(true),
                )
                .changed()
            {
                let _ = &mut *trans;
            }
            if ui
                .add(
                    egui::Slider::new(
                        &mut trans.bypass_change_detection().translation.z,
                        -10.0..=10.0,
                    )
                    .text("Z")
                    .clamping(egui::SliderClamping::Never),
                )
                .changed()
            {
                let _ = &mut *trans;
            }
            let layer = healpix::Layer::new(params.depth);
            let center = layer.center(containing.0);
            let LonLat { lon, lat } =
                get_absolute(center, trans.translation.xz().as_dvec2() / SCALE);
            let (mut new_lon, mut new_lat) = (lon, lat);
            ui.add(egui::Slider::new(&mut new_lon, 0.0..=TAU).text("Longitude"));
            ui.add(egui::Slider::new(&mut new_lat, -FRAC_PI_2..=FRAC_PI_2).text("Latitude"));
            if lon != new_lon || lat != new_lat {
                let off =
                    (get_relative(center, LonLat::from_f64(new_lon, new_lat)) * SCALE).as_vec2();
                trans.translation.x = off.x;
                trans.translation.z = off.y;
            }
        });
        egui::Window::new("Misc").show(ctx, |ui| {
            if ui
                .add(
                    egui::Slider::new(&mut params.bypass_change_detection().delta, 0..=5)
                        .text("Loading Delta"),
                )
                .changed()
            {
                let _ = &mut *params;
            }
            if ui
                .add(
                    egui::Slider::new(&mut speed.bypass_change_detection().0, 0.0..=1000.0)
                        .logarithmic(true)
                        .text("Movement Speed"),
                )
                .changed()
            {
                let _ = &mut *speed;
            }
            if ui
                .checkbox(&mut tests.bypass_change_detection().0, "Test Points")
                .changed()
            {
                let _ = &mut *tests;
            }
            let mut fallback = false;
            let enabled = wireframe.is_some();
            let mref = wireframe.map_or(&mut fallback, |f| &mut f.into_inner().global);
            ui.add_enabled(enabled, egui::Checkbox::new(mref, "Wireframes"))
                .changed();
        });
        egui::Window::new("Cells").show(ctx, |ui| {
            ui.label(format!("{} cells loaded", cells.iter().len()));
            egui::ScrollArea::vertical().show(ui, |ui| {
                for (OwningCell(cell), trans, gtrans, despawn) in cells.iter() {
                    ui.collapsing(
                        format!(
                            "{cell}{}",
                            if *cell == containing.0 {
                                "- Containing"
                            } else {
                                ""
                            }
                        ),
                        |ui| {
                            if let Some(DespawnTime(despawn)) = despawn {
                                if let Some(remaining) = despawn.checked_sub(time.elapsed()) {
                                    ui.label(format!("Queued to despawn in {remaining:?}"));
                                } else {
                                    ui.label("Despawn imminent!");
                                }
                            }
                            ui.collapsing("Transform", |ui| {
                                let mat = trans.compute_matrix().transpose();
                                ui.label(
                                    egui::WidgetText::from(egui::text::LayoutJob::simple(
                                        format!(
                                            "Translation: {:10.6?}\nRotation:    {:10.6?}\nScale:       {:10.6?}\nMatrix:\n{:10.6?}\n{:10.6?}\n{:10.6?}\n{:10.6?}",
                                            trans.translation.to_array(), trans.rotation.to_array(), trans.scale.to_array(),
                                            mat.x_axis.to_array(), mat.y_axis.to_array(), mat.z_axis.to_array(), mat.w_axis.to_array()
                                        ),
                                        egui::FontId::monospace(ui.text_style_height(&egui::TextStyle::Small)),
                                        ui.style().visuals.text_color(),
                                        f32::INFINITY
                                    )).into_galley(ui, Some(egui::TextWrapMode::Extend), 0.0, egui::FontSelection::Default)
                                );
                            });
                            ui.collapsing("Global Transform", |ui| {
                                let trans = gtrans.compute_transform();
                                let mat = trans.compute_matrix().transpose();
                                ui.label(
                                    egui::WidgetText::from(egui::text::LayoutJob::simple(
                                        format!(
                                            "Translation: {:10.6?}\nRotation:    {:10.6?}\nScale:       {:10.6?}\nMatrix:\n{:10.6?}\n{:10.6?}\n{:10.6?}\n{:10.6?}",
                                            trans.translation.to_array(), trans.rotation.to_array(), trans.scale.to_array(),
                                            mat.x_axis.to_array(), mat.y_axis.to_array(), mat.z_axis.to_array(), mat.w_axis.to_array()
                                        ),
                                        egui::FontId::monospace(ui.text_style_height(&egui::TextStyle::Small)),
                                        ui.style().visuals.text_color(),
                                        f32::INFINITY
                                    )).into_galley(ui, Some(egui::TextWrapMode::Extend), 0.0, egui::FontSelection::Default)
                                );
                            });
                        },
                    );
                }
            })
        });
    }
}

fn update_coords(
    mut objects: Query<(&mut Transform, &mut OwningCell)>,
    params: Res<HealpixParams>,
    mut old_depth: Local<Option<u8>>,
) {
    let Some(old) = *old_depth else {
        *old_depth = Some(params.depth);
        return;
    };
    if old == params.depth {
        return;
    }
    let old_layer = healpix::Layer::new(old);
    let new_layer = healpix::Layer::new(params.depth);
    for (mut trans, mut cell) in objects.iter_mut() {
        let old = old_layer.center(cell.0);
        let abs = get_absolute(old, trans.translation.xz().as_dvec2() / SCALE);
        cell.0 = new_layer.hash(abs);
        let new = new_layer.center(cell.0);
        let off = (get_relative(new, abs) * SCALE).as_vec2();
        trans.translation.x = off.x;
        trans.translation.z = off.y;
    }
    *old_depth = Some(params.depth);
}

fn check_cell(
    mut commands: Commands,
    mut objects: Query<
        (Entity, &mut Transform, &mut OwningCell, Has<Camera3d>),
        (
            Or<(Changed<Transform>, Changed<OwningCell>)>,
            Without<CellCenter>,
        ),
    >,
    cells: Query<(Entity, &OwningCell), With<CellCenter>>,
    params: Res<HealpixParams>,
) {
    let layer = healpix::Layer::new(params.depth);
    for (entity, mut trans, mut cell, is_cam) in objects.iter_mut() {
        let old = layer.center(cell.0);
        let abs = get_absolute(old, trans.translation.xz().as_dvec2() / SCALE);
        let new_hash = layer.hash(abs);
        if new_hash == cell.0 {
            continue;
        }
        let new = layer.center(new_hash);
        let off = (get_relative(new, abs) * SCALE).as_vec2();
        trans.translation.x = off.x;
        trans.translation.z = off.y;
        cell.0 = new_hash;
        if is_cam {
            continue;
        }
        let Some((new_parent, _)) = cells.iter().find(|c| c.1 .0 == new_hash) else {
            panic!("Cell {new_hash} doesn't exist but an entity wants to be parented to it!");
        };
        commands.entity(entity).set_parent(new_parent);
    }
}

fn grab_mouse(mut window: Query<&mut Window>, locked: Res<LockedMouse>) {
    for mut window in window.iter_mut() {
        if locked.0 {
            info!("grabbing mouse");
            window.cursor.grab_mode = CursorGrabMode::Confined;
            window.cursor.visible = false;
        } else {
            info!("releasing mouse");
            window.cursor.grab_mode = CursorGrabMode::None;
            window.cursor.visible = true;
        }
    }
}

fn spawn_cell(
    cell: In<(u64, Entity)>,
    mut commands: Commands,
    params: Res<HealpixParams>,
    assets: Res<AssetServer>,
    mut translate: Local<f32>,
) {
    let (cell, entity) = cell.0;
    let corners = corners_of(params.depth, cell);
    info!(hash = cell, ?corners, %entity, "initializing cell");
    let (min_x, max_x, min_y, max_y) = corners.iter().fold(
        (
            f32::INFINITY,
            f32::NEG_INFINITY,
            f32::INFINITY,
            f32::NEG_INFINITY,
        ),
        |(ix, ax, iy, ay), &Vec2 { x, y }| (ix.min(x), ax.max(x), iy.min(y), ay.max(y)),
    );
    let scale_x = (max_x - min_x).recip();
    let scale_y = (max_y - min_y).recip();
    let mesh = Mesh::new(
        bevy::render::mesh::PrimitiveTopology::TriangleList,
        RenderAssetUsages::all(),
    )
    .with_inserted_attribute(
        Mesh::ATTRIBUTE_POSITION,
        corners.map(|v| v.extend(0.0).xzy()).to_vec(),
    )
    .with_inserted_attribute(
        Mesh::ATTRIBUTE_UV_0,
        corners
            .map(|v| [(v.x - min_x) * scale_x, (v.y - min_y) * scale_y])
            .to_vec(),
    )
    .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, vec![Vec3::Y; 4])
    .with_inserted_indices(bevy::render::mesh::Indices::U16(vec![2, 3, 1, 1, 3, 0]));
    let size = Extent3d {
        width: 512,
        height: 512,
        ..default()
    };
    let mut image = Image {
        texture_descriptor: TextureDescriptor {
            label: None,
            size,
            dimension: TextureDimension::D2,
            format: TextureFormat::Bgra8UnormSrgb,
            mip_level_count: 1,
            sample_count: 1,
            usage: TextureUsages::TEXTURE_BINDING
                | TextureUsages::COPY_DST
                | TextureUsages::RENDER_ATTACHMENT,
            view_formats: &[],
        },
        ..default()
    };
    image.resize(size);
    let handle = assets.add(image);
    commands
        .entity(entity)
        .insert((
            PbrBundle {
                mesh: assets.add(mesh),
                material: assets.add(StandardMaterial {
                    base_color: Color::LinearRgba(LinearRgba::WHITE),
                    base_color_texture: Some(handle.clone()),
                    ..default()
                }),
                ..default()
            },
            RenderLayers::layer(0),
            OwningCell(cell),
            CellCenter,
        ))
        .with_children(|commands| {
            commands.spawn((
                Camera2dBundle {
                    camera: Camera {
                        target: bevy::render::camera::RenderTarget::Image(handle.clone()),
                        ..default()
                    },
                    transform: Transform::from_xyz(*translate, 0.0, 0.0),
                    ..default()
                },
                RenderLayers::layer(1),
            ));
            commands.spawn((
                Text2dBundle {
                    text: Text::from_section(cell.to_string(), default()),
                    transform: Transform::from_xyz(*translate, 0.0, 0.0),
                    ..default()
                },
                RenderLayers::layer(1),
            ));
            *translate += 10000.0;
            // let scale = thread_rng().gen_range(0.1..=5.0);
            let scale = 1.0;
            commands.spawn(PbrBundle {
                mesh: assets.add(Cuboid::new(scale, scale, scale).mesh().build()),
                // transform: Transform::from_rotation(thread_rng().gen())
                //     .with_translation(Vec3::Y * scale * 2.0),
                transform: Transform::from_translation(Vec3::Y * scale * 2.0),
                ..default()
            });
        });
}

fn load_cells(
    mut commands: Commands,
    camera: Query<(&OwningCell, &Transform), With<Camera3d>>,
    mut cells: Query<
        (Entity, &mut Visibility, &mut Transform, &OwningCell),
        (With<CellCenter>, Without<Camera3d>),
    >,
    params: Res<HealpixParams>,
    time: Res<Time>,
    systems: Res<Systems>,
) {
    let (&OwningCell(center), camera) = camera.single();
    let base_layer = healpix::Layer::new(params.depth);
    let abs = get_absolute(
        base_layer.center(center),
        camera.translation.xz().as_dvec2() / SCALE,
    );
    let delta_layer = healpix::Layer::new(params.depth + params.delta);
    let delta_hash = delta_layer.hash(abs);
    let mut set = tinyset::SetU64::new();
    let elapsed = time.elapsed();
    for (entity, mut vis, mut trans, OwningCell(cell)) in cells.iter_mut() {
        set.insert(*cell);
        if *cell == center {
            commands.entity(entity).remove::<DespawnTime>();
            *vis = Visibility::Visible;
            *trans = BASE_TRANSFORM;
        } else if base_layer
            .external_edge(*cell, params.delta)
            .contains(&delta_hash)
        {
            commands.entity(entity).remove::<DespawnTime>();
            let mat = transforms_for(params.depth, center, *cell);
            *vis = Visibility::Visible;
            *trans = mat;
        } else {
            commands
                .entity(entity)
                .add(move |mut entity: EntityWorldMut| {
                    if !entity.contains::<DespawnTime>() {
                        entity.insert(DespawnTime(elapsed + Duration::from_secs(5)));
                    }
                });
            *vis = Visibility::Hidden;
        }
    }
    if !set.contains(center) {
        let id = commands
            .spawn((
                CellCenter,
                OwningCell(center),
                Transform::default(),
                Visibility::default(),
            ))
            .id();
        info!(%id, hash = center, "spawning cell");
        commands.run_system_with_input(systems.spawn_cell, (center, id));
    }
    for &cell in &base_layer.neighbors_slice(center, false) {
        if set.contains(cell) {
            continue;
        }
        if base_layer
            .external_edge(cell, params.delta)
            .contains(&delta_hash)
        {
            let id = commands
                .spawn((
                    CellCenter,
                    OwningCell(cell),
                    Transform::default(),
                    Visibility::default(),
                ))
                .id();
            info!(%id, hash = cell, "spawning cell");
            commands.run_system_with_input(systems.spawn_cell, (cell, id));
        }
    }
}

fn render_axes(mut gizmos: Gizmos, camera: Query<&GlobalTransform, With<Camera3d>>) {
    let camera = camera.single();
    let center = camera.transform_point(-Vec3::Z);
    gizmos.line(center, center + Vec3::X * 0.1, LinearRgba::RED);
    gizmos.line(center, center + Vec3::Y * 0.1, LinearRgba::GREEN);
    gizmos.line(center, center + Vec3::Z * 0.1, LinearRgba::BLUE);
}

fn despawn_system(mut commands: Commands, time: Res<Time>, query: Query<(Entity, &DespawnTime)>) {
    for (entity, despawn) in query.iter() {
        if despawn.0 > time.elapsed() {
            info!(%entity, "despawning entity");
            commands.entity(entity).despawn_recursive();
        }
    }
}

fn free_mouse(mut locked: ResMut<LockedMouse>, mut evt: EventReader<WindowFocused>) {
    if let Some(WindowFocused { focused: false, .. }) = evt.read().last() {
        locked.0 = false;
    }
}

fn nan_checks(query: Query<(Entity, &Transform)>) {
    for (entity, trans) in query.iter() {
        assert!(
            trans.is_finite(),
            "entity {entity} has a non-finite transform!"
        );
    }
}

fn test_points(
    mut gizmos: Gizmos,
    camera: Query<&OwningCell, With<Camera3d>>,
    params: Res<HealpixParams>,
) {
    let &OwningCell(cell) = camera.single();
    let layer = healpix::Layer::new(params.depth);
    let center = layer.center(cell);
    for dx in (-40..=40).map(|x| x as f64 * 0.5) {
        for dy in (-40..=40).map(|x| x as f64 * 0.5) {
            let coords = get_absolute(center, DVec2::new(dx, dy) / SCALE);
            if layer.hash(coords) == cell {
                gizmos.sphere(
                    Vec3::new(dx as _, 1.0, dy as _),
                    Quat::IDENTITY,
                    0.01,
                    Color::WHITE,
                );
            }
        }
    }
}
