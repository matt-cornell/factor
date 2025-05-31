#![feature(array_chunks)]

use bevy::ecs::system::SystemId;
use bevy::input::mouse::MouseMotion;
use bevy::math::DVec2;
use bevy::pbr::wireframe::{WireframeConfig, WireframePlugin};
use bevy::prelude::*;
use bevy::render::render_resource::{
    Extent3d, TextureDescriptor, TextureDimension, TextureFormat, TextureUsages,
};
use bevy::render::view::RenderLayers;
use bevy::window::CursorGrabMode;
use bevy::window::WindowFocused;
use bevy_egui::{egui, EguiContexts, EguiPlugin};
use factor_common::cell::*;
use factor_common::{geo, PLANET_RADIUS};
use healpix::LonLat;
use rand::Rng;
use std::cmp::Ordering;
use std::f64::consts::{FRAC_PI_2, TAU};
use std::time::Duration;

const BASE_TRANSFORM: Transform = Transform::IDENTITY;

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
    spawn_cell: SystemId<In<(u64, Entity)>, ()>,
}

#[derive(Debug, Clone, Copy, Resource)]
struct HealpixParams {
    depth: u8,
    delta: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Resource)]
struct ShowTestPoints(bool);

fn mesh_quad(corners: [Vec2; 4], depth: usize) -> Mesh {
    let len = depth + 1;
    let nverts = len * len;
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
    let uv_add = Vec2::new(-min_x, -min_y);
    let uv_scale = Vec2::new(scale_x, scale_y);
    let mut vertices = vec![Vec3::NAN; nverts];
    let mut uv = vec![Vec2::NAN; nverts];
    let mut triangles = Vec::with_capacity(2 * nverts);
    let [v0, v1, v2, v3] = corners;
    let va = v1 - v0;
    let vb = v2 - v0;
    let vc = v3 - v0;
    let scale = (depth as f32).recip();
    for i in 0..len {
        for j in 0..len {
            let idx = i * len + j;
            let basis = match i.cmp(&j) {
                Ordering::Less => {
                    triangles.extend_from_slice(&[
                        [idx as u32, (idx - 1) as u32, (idx + len) as u32],
                        [(idx - 1) as u32, (idx + len - 1) as u32, (idx + len) as u32],
                    ]);
                    va * scale * i.abs_diff(j) as f32
                }
                Ordering::Greater => {
                    triangles.extend_from_slice(&[
                        [idx as u32, (idx + 1) as u32, (idx - len) as u32],
                        [(idx + 1) as u32, (idx - len + 1) as u32, (idx - len) as u32],
                    ]);
                    vc * scale * i.abs_diff(j) as f32
                }
                Ordering::Equal => Vec2::ZERO,
            };
            let b_basis = vb * scale * i.min(j) as f32;
            let vert = b_basis + basis + v0;
            vertices[idx] = vert.extend(0.0).xzy();
            uv[idx] = (vert + uv_add) * uv_scale;
        }
    }
    factor_common::mesh::MeshData {
        vertices,
        triangles,
        uv,
    }
    .build_bevy()
}

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
        .insert_resource(HealpixParams {
            depth: 12,
            delta: 0,
        })
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
        Camera3d::default(),
        Transform::from_xyz(1.0, 20.0, -1.0).looking_at(Vec3::ZERO, Dir3::Y),
        RenderLayers::layer(0),
        OwningCell(0),
    ));
    commands.spawn((
        PointLight {
            intensity: 400.0,
            range: 2000.0,
            radius: 10.0,
            shadows_enabled: true,
            ..default()
        },
        Transform::from_xyz(0.0, 30.0, 0.0),
    ));
    let spawn_cell = commands.register_system(spawn_cell);
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
                let layer = healpix::get(params.depth);
                let LonLat { lon, lat } = geo::absolute(
                    layer.center(cell.0),
                    trans.translation.xz().as_dvec2() / PLANET_RADIUS,
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
                        0..=(healpix::checked::n_hash(params.depth) - 1),
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
            let layer = healpix::get(params.depth);
            let center = layer.center(containing.0);
            let LonLat { lon, lat } =
                geo::absolute(center, trans.translation.xz().as_dvec2() / PLANET_RADIUS);
            let (mut new_lon, mut new_lat) = (lon, lat);
            ui.add(egui::Slider::new(&mut new_lon, 0.0..=TAU).text("Longitude"));
            ui.add(egui::Slider::new(&mut new_lat, -FRAC_PI_2..=FRAC_PI_2).text("Latitude"));
            if lon != new_lon || lat != new_lat {
                let off = (geo::relative(center, [new_lon, new_lat]) * PLANET_RADIUS).as_vec2();
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
            let enabled = !cfg!(target_family = "wasm") && wireframe.is_some();
            let mref = wireframe.map_or(&mut fallback, |f| &mut f.into_inner().global);
            ui.add_enabled(enabled, egui::Checkbox::new(mref, "Wireframes"))
                .on_disabled_hover_text("Wireframes are disabled here!")
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
    let old_layer = healpix::get(old);
    let new_layer = healpix::get(params.depth);
    for (mut trans, mut cell) in objects.iter_mut() {
        let old = old_layer.center(cell.0);
        let abs = geo::absolute(old, trans.translation.xz().as_dvec2() / PLANET_RADIUS);
        cell.0 = new_layer.hash(abs);
        let new = new_layer.center(cell.0);
        let off = (DVec2::from(healpix::geo::relative(new, abs)) * PLANET_RADIUS).as_vec2();
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
    let layer = healpix::get(params.depth);
    for (entity, mut trans, mut cell, is_cam) in objects.iter_mut() {
        let old = layer.center(cell.0);
        let abs = geo::absolute(old, trans.translation.xz().as_dvec2() / PLANET_RADIUS);
        let new_hash = layer.hash(abs);
        if new_hash == cell.0 {
            continue;
        }
        let new = layer.center(new_hash);
        let off = (geo::relative(new, abs) * PLANET_RADIUS).as_vec2();
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
            window.cursor_options.grab_mode = CursorGrabMode::Confined;
            window.cursor_options.visible = false;
        } else {
            info!("releasing mouse");
            window.cursor_options.grab_mode = CursorGrabMode::None;
            window.cursor_options.visible = true;
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
    let mut corners = corners_of(params.depth, cell);
    corners.reverse();
    let area = -0.5
        * ((corners[0].x * corners[1].y
            + corners[1].x * corners[2].y
            + corners[2].x * corners[3].y
            + corners[3].x * corners[0].y)
            - (corners[1].x * corners[0].y
                + corners[2].x * corners[1].y
                + corners[3].x * corners[2].y
                + corners[0].x * corners[3].y));
    info!(hash = cell, area, ?corners, %entity, "initializing cell");

    let mesh = mesh_quad(corners, 1).with_computed_normals();
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
            Mesh3d(assets.add(mesh)),
            MeshMaterial3d(assets.add(StandardMaterial {
                base_color: Color::LinearRgba(LinearRgba::WHITE),
                base_color_texture: Some(handle.clone()),
                ..default()
            })),
            RenderLayers::layer(0),
            OwningCell(cell),
            CellCenter,
        ))
        .with_children(|commands| {
            commands.spawn((
                Camera2d,
                Camera {
                    target: bevy::render::camera::RenderTarget::Image(handle.clone()),
                    ..default()
                },
                Transform::from_xyz(*translate, 0.0, 0.0),
                RenderLayers::layer(1),
            ));
            commands.spawn((
                Text2d(cell.to_string()),
                Transform::from_xyz(*translate, 0.0, 0.0),
                RenderLayers::layer(1),
            ));
            *translate += 10000.0;
            let scale = 1.0;
            commands.spawn((
                Mesh3d(assets.add(Cuboid::new(scale, scale, scale).mesh().build())),
                MeshMaterial3d(assets.add(StandardMaterial {
                    base_color: Color::hsv(rand::thread_rng().gen_range(0.0..=360.0), 1.0, 1.0),
                    unlit: true,
                    ..default()
                })),
                Transform::from_translation(Vec3::Y * scale * 2.0),
            ));
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
    let base_layer = healpix::get(params.depth);
    let abs = geo::absolute(
        base_layer.center(center),
        camera.translation.xz().as_dvec2() / PLANET_RADIUS,
    );
    let delta_layer = healpix::get(params.depth + params.delta);
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
            .external_edge(*cell, params.delta, false)
            .contains(&delta_hash)
        {
            commands.entity(entity).remove::<DespawnTime>();
            let mat = transforms_for(params.depth, center, *cell);
            *vis = Visibility::Visible;
            *trans = mat;
        } else {
            commands
                .entity(entity)
                .queue(move |mut entity: EntityWorldMut| {
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
    for &cell in base_layer.neighbors(center).values() {
        if set.contains(cell) {
            continue;
        }
        if base_layer
            .external_edge(cell, params.delta, false)
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
    let layer = healpix::get(params.depth);
    let center = layer.center(cell);
    for dx in (-40..=40).map(|x| x as f64 * 0.5) {
        for dy in (-40..=40).map(|x| x as f64 * 0.5) {
            let coords = geo::absolute(center, DVec2::new(dx, dy) / PLANET_RADIUS);
            if layer.hash(coords) == cell {
                gizmos.sphere(
                    Isometry3d::from_xyz(dx as _, 1.0, dy as _),
                    0.01,
                    Color::WHITE,
                );
            }
        }
    }
}
