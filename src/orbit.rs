use bevy::prelude::*;

use crate::config::WorldConfig;

/// Marker component for the center of the planet.
///
/// This transform won't have its rotation updated, only its position.
#[derive(Debug, Clone, Copy, Component)]
pub struct PlanetCenter;

/// Marker component for the planet.
///
/// This is expected to be a child of a [`PlanetCenter`], and so only its rotation is updated.
#[derive(Debug, Clone, Copy, Component)]
pub struct PlanetSurface;

/// System to update the transforms of the planets.
pub fn update_planet_transforms(
    mut center: Query<&mut Transform, With<PlanetCenter>>,
    mut planet: Query<&mut Transform, (With<PlanetSurface>, Without<PlanetCenter>)>,
    time: Res<Time>,
    params: Res<WorldConfig>,
    mut last_tilt: Local<f32>,
) {
    use std::f32::consts::TAU;
    let delta = time.delta_seconds();
    for mut center in center.iter_mut() {
        center.translation = Quat::from_rotation_z(delta / params.orbit.year_length * TAU)
            .mul_vec3(center.translation);
    }
    for mut planet in planet.iter_mut() {
        planet.rotation = Quat::from_rotation_y(params.orbit.obliquity - *last_tilt)
            * planet.rotation
            * Quat::from_rotation_z(delta / params.orbit.day_length * TAU);
    }
    *last_tilt = params.orbit.obliquity;
}

#[derive(Debug, Clone, Copy)]
pub struct OrbitPlugin {
    /// Should the entitites be set up? If so, how far away should the planet be?
    pub setup_planets: Option<f32>,
}
impl Default for OrbitPlugin {
    fn default() -> Self {
        Self {
            setup_planets: Some(1.0),
        }
    }
}
impl Plugin for OrbitPlugin {
    fn build(&self, app: &mut App) {
        if let Some(dist) = self.setup_planets {
            let surface = app
                .world_mut()
                .spawn((TransformBundle::default(), PlanetSurface))
                .id();
            app.world_mut()
                .spawn((
                    TransformBundle {
                        local: Transform::from_xyz(dist, 0.0, 0.0),
                        ..default()
                    },
                    PlanetCenter,
                ))
                .add_child(surface);
        }
        app.add_systems(Update, update_planet_transforms);
    }
}
