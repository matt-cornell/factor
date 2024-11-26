#![feature(array_chunks)]
#![allow(clippy::type_complexity)]

use bevy::ecs::system::SystemId;
use bevy::input::mouse::MouseMotion;
use bevy::prelude::*;
use bevy::render::render_asset::RenderAssetUsages;
use bevy::window::CursorGrabMode;
use bevy_egui::{egui, EguiContexts, EguiPlugin};
use factor_common::coords::*;
use factor_common::healpix;
use std::convert::Infallible;
use std::f32::consts::{FRAC_PI_2, TAU};
use std::sync::{LazyLock, OnceLock};
use std::time::Duration;
use triomphe::Arc;

static CACHE: [LazyLock<
    quick_cache::sync::Cache<u64, ([Vec2; 4], Arc<[(u64, OnceLock<Mat4>); 8]>)>,
>; 29] = [const { LazyLock::new(|| quick_cache::sync::Cache::new(65536)) }; 29];
fn cache_slice(depth: u8, hash: u64) -> ([Vec2; 4], Arc<[(u64, OnceLock<Mat4>); 8]>) {
    let Ok(res) = CACHE[depth as usize].get_or_insert_with(&hash, || {
        let layer = healpix::Layer::new(depth);
        let vertices = layer.vertices(hash);
        let center = layer.center(hash);
        let corners = vertices.map(|c2| get_relative(center, c2));
        let slice = &healpix::neighbors_list(depth)[(hash as usize * 8)..(hash as usize * 8 + 8)];
        let neighbors = <[u64; 8]>::try_from(slice)
            .unwrap()
            .map(|i| (i, OnceLock::new()));
        Ok::<_, Infallible>((corners, Arc::new(neighbors)))
    });
    res
}
fn corners_of(depth: u8, hash: u64) -> [Vec2; 4] {
    cache_slice(depth, hash).0
}
fn transforms_for(depth: u8, base: u64, neighbor: u64) -> Mat4 {
    let slice = cache_slice(depth, base);
    let Some(cell) = slice
        .1
        .iter()
        .find_map(|(n, c)| (*n == neighbor).then_some(c))
    else {
        panic!("Cell {neighbor} doesn't neighbor {base}!");
    };

    *cell.get_or_init(|| {
        let layer = healpix::Layer::new(depth);
        let vertices = layer.vertices(neighbor);
        let from = Mat4::from_cols_array_2d(
            &corners_of(depth, neighbor).map(|v| v.extend(0.0).extend(1.0).to_array()),
        );
        let to = {
            let center = layer.center(base);
            Mat4::from_cols_array_2d(
                &vertices.map(|c2| get_relative(center, c2).extend(0.0).extend(1.0).to_array()),
            )
        };
        to.mul_mat4(&from.inverse())
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
        .insert_resource(HealpixParams { depth: 5, delta: 2 })
        .insert_resource(LockedMouse(false))
        .insert_resource(MovementSpeed(0.001))
        .add_systems(Startup, setup)
        .add_systems(FixedUpdate, handle_input)
        .add_systems(
            Update,
            (
                show_ui,
                check_cell,
                despawn_system,
                load_cells,
                update_coords.run_if(resource_changed::<HealpixParams>),
                grab_mouse.run_if(resource_changed::<LockedMouse>),
            ),
        )
        .run();
}

fn setup(mut commands: Commands) {
    let base = commands
        .spawn_empty()
        .with_children(|commands| {
            commands.spawn((
                Camera3dBundle {
                    transform: Transform::from_xyz(0.0, 0.1, 0.0)
                        .looking_at(Vec3::ZERO, Dir3::Y)
                        .with_scale(Vec3::splat(1000.0)),
                    ..default()
                },
                OwningCell(0),
            ));
        })
        .id();
    commands.spawn(PointLightBundle {
        point_light: PointLight {
            intensity: 400.0,
            range: 2000.0,
            radius: 10.0,
            shadows_enabled: true,
            ..default()
        },
        transform: Transform::from_xyz(0.0, 1000.0, 0.0).with_scale(Vec3::splat(100.0)),
        ..default()
    });
    let spawn_cell = commands.register_one_shot_system(spawn_cell);
    commands.run_system_with_input(spawn_cell, (0, base));
    commands.insert_resource(Systems { spawn_cell });
}

fn handle_input(
    mut locked: ResMut<LockedMouse>,
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
    if keys.pressed(KeyCode::Space) {
        velocity += Vec3::Y * speed.0;
    }
    if keys.pressed(KeyCode::ShiftLeft) {
        velocity -= Vec3::Y * speed.0;
    }
    let forward = camera
        .forward()
        .xz()
        .normalize_or(camera.up().xz())
        .extend(0.0)
        .xzy();
    let right = camera.right();
    if keys.pressed(KeyCode::KeyW) {
        velocity += forward * speed.0;
    }
    if keys.pressed(KeyCode::KeyA) {
        velocity -= right * speed.0;
    }
    if keys.pressed(KeyCode::KeyS) {
        velocity -= forward * speed.0;
    }
    if keys.pressed(KeyCode::KeyD) {
        velocity += right * speed.0;
    }
    camera.translation += velocity;
    for &MouseMotion {
        delta: Vec2 { x, y },
    } in mouse.read()
    {
        camera.rotate_local_y(x * 0.01);
        let angle = camera.up().y.asin();
        let new_angle = (angle + y * 0.01).clamp(-FRAC_PI_2, FRAC_PI_2);
        camera.rotate_local_x(new_angle - angle);
    }
}

fn show_ui(
    mut contexts: EguiContexts,
    locked: Res<LockedMouse>,
    mut params: ResMut<HealpixParams>,
    mut camera: Query<(&mut Transform, &mut OwningCell), With<Camera3d>>,
) {
    let ctx = contexts.ctx_mut();
    if locked.0 {
        egui::Area::new(egui::Id::new("HUD"))
            .anchor(egui::Align2::LEFT_TOP, egui::vec2(10.0, 10.0))
            .show(ctx, |ui| {
                let (trans, cell) = camera.single();
                let layer = healpix::Layer::new(params.depth);
                let LonLat { lon, lat } =
                    get_absolute(layer.center(cell.0), trans.translation.xz());
                ui.label(format!(
                    "Cell: {}\nX: {:.5}, Y: {:.5}, Z: {:.5}\nLon: {lon:.3}, Lat: {lat:.3}",
                    cell.0, trans.translation.x, trans.translation.y, trans.translation.z,
                ));
            });
    } else {
        egui::Window::new("Position").show(ctx, |ui| {
            let (mut trans, cell) = camera.single_mut();
            ui.label(format!("Cell: {}", cell.0));
            if ui
                .add(
                    egui::Slider::new(
                        &mut trans.bypass_change_detection().translation.x,
                        -1.0..=1.0,
                    )
                    .text("X"),
                )
                .changed()
            {
                let _ = &mut *trans;
            }
            if ui
                .add(
                    egui::Slider::new(
                        &mut trans.bypass_change_detection().translation.y,
                        -1.0..=1.0,
                    )
                    .text("Y"),
                )
                .changed()
            {
                let _ = &mut *trans;
            }
            if ui
                .add(
                    egui::Slider::new(
                        &mut trans.bypass_change_detection().translation.z,
                        -1.0..=1.0,
                    )
                    .text("Z"),
                )
                .changed()
            {
                let _ = &mut *trans;
            }
            let layer = healpix::Layer::new(params.depth);
            let center = layer.center(cell.0);
            let LonLat { lon, lat } = get_absolute(center, trans.translation.xz());
            let (mut new_lon, mut new_lat) = (lon, lat);
            ui.add(egui::Slider::new(&mut new_lon, 0.0..=TAU).text("Longitude"));
            ui.add(egui::Slider::new(&mut new_lat, -FRAC_PI_2..=FRAC_PI_2).text("Latitude"));
            if lon != new_lon || lat != new_lat {
                let off = get_relative(center, LonLat::new(new_lon, new_lat));
                trans.translation.x = off.x;
                trans.translation.z = off.y;
            }
        });
        egui::Window::new("Healpix").show(ctx, |ui| {
            if ui
                .add(
                    egui::Slider::new(&mut params.bypass_change_detection().depth, 0..=29)
                        .text("Healpix Depth"),
                )
                .changed()
            {
                let _ = &mut *params;
            }
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
        let abs = get_absolute(old, trans.translation.xz());
        cell.0 = new_layer.hash(abs);
        let new = new_layer.center(cell.0);
        let off = get_relative(new, abs);
        trans.translation.x = off.x;
        trans.translation.z = off.y;
    }
    *old_depth = Some(params.depth);
}

fn check_cell(
    mut objects: Query<
        (&mut Transform, &mut OwningCell),
        Or<(Changed<Transform>, Changed<OwningCell>)>,
    >,
    params: Res<HealpixParams>,
) {
    let layer = healpix::Layer::new(params.depth);
    for (mut trans, mut cell) in objects.iter_mut() {
        let old = layer.center(cell.0);
        let abs = get_absolute(old, trans.translation.xz());
        let new_hash = layer.hash(abs);
        if new_hash == cell.0 {
            continue;
        }
        let new = layer.center(new_hash);
        let off = get_relative(new, abs);
        trans.translation.x = off.x;
        trans.translation.z = off.y;
        cell.0 = new_hash;
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
        |(ix, iy, ax, ay), &Vec2 { x, y }| (ix.min(x), ax.max(x), iy.min(y), ay.max(y)),
    );
    let scale_x = (max_x - min_x).recip();
    let scale_y = (max_y - min_y).recip();
    let add_x = -min_x * scale_x;
    let add_y = -min_y * scale_y;
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
            .map(|v| [v.x.mul_add(scale_x, add_x), v.y.mul_add(scale_y, add_y)])
            .to_vec(),
    )
    // .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, vec![Vec3::Y; 4])
    .with_inserted_indices(bevy::render::mesh::Indices::U16(vec![0, 3, 1, 1, 3, 2]));
    commands.entity(entity).insert((
        PbrBundle {
            mesh: assets.add(mesh),
            ..default()
        },
        OwningCell(cell),
        CellCenter,
    ));
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
    let abs = get_absolute(base_layer.center(center), camera.translation.xz());
    let delta_layer = healpix::Layer::new(params.depth + params.delta);
    let delta_hash = delta_layer.hash(abs);
    let mut set = tinyset::SetU64::new();
    let elapsed = time.elapsed();
    for (entity, mut vis, mut trans, OwningCell(cell)) in cells.iter_mut() {
        set.insert(*cell);
        if *cell == center {
            commands.entity(entity).remove::<DespawnTime>();
            *vis = Visibility::Visible;
            *trans = Transform::IDENTITY;
        } else if base_layer
            .external_edge(*cell, params.delta)
            .contains(&delta_hash)
        {
            commands.entity(entity).remove::<DespawnTime>();
            *vis = Visibility::Visible;
            *trans = Transform::from_matrix(transforms_for(params.depth, center, *cell));
        } else {
            commands
                .entity(entity)
                .add(move |mut entity: EntityWorldMut| {
                    if !entity.contains::<DespawnTime>() {
                        entity.insert(DespawnTime(elapsed));
                    }
                });
            *vis = Visibility::Hidden;
        }
    }
    for &i in base_layer.neighbors_slice(center) {
        if set.contains(i) {
            continue;
        }
        if base_layer
            .external_edge(i, params.delta)
            .contains(&delta_hash)
        {
            let id = commands.spawn_empty().id();
            info!(%id, hash = i, "spawning cell");
            commands.run_system_with_input(systems.spawn_cell, (i, id));
        }
    }
}

fn despawn_system(mut commands: Commands, time: Res<Time>, query: Query<(Entity, &DespawnTime)>) {
    for (entity, despawn) in query.iter() {
        if despawn.0 > time.elapsed() {
            debug!(%entity, "despawning entity");
            commands.entity(entity).despawn();
        }
    }
}