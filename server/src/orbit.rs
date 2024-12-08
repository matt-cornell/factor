use crate::config::WorldConfig;
use bevy::prelude::*;

/// Marker component for the center of the planet.
///
/// This transform won't have its rotation updated, only its position.
#[derive(Debug, Default, Clone, Copy, Component)]
pub struct PlanetCenter;

/// Marker component for the planet.
///
/// This is expected to be a child of a [`PlanetCenter`], and so only its rotation is updated.
#[derive(Debug, Default, Clone, Copy, Component)]
pub struct PlanetSurface {
    pub last_tilt: f32,
}

#[derive(Debug, Clone, Copy, PartialEq, Resource)]
pub struct OrbitRunning(pub bool);

/// System to update the transforms of the planets.
pub fn update_planet_transforms(
    In(delta): In<f32>,
    mut center: Query<&mut Transform, With<PlanetCenter>>,
    mut planet: Query<(&mut Transform, &mut PlanetSurface), Without<PlanetCenter>>,
    params: Res<WorldConfig>,
) {
    use std::f32::consts::TAU;
    for mut center in center.iter_mut() {
        center.translation = Quat::from_rotation_z(delta / params.orbit.year_length * TAU)
            .mul_vec3(center.translation);
    }
    for (mut planet, mut surface) in planet.iter_mut() {
        planet.rotation = Quat::from_rotation_y(params.orbit.obliquity - surface.last_tilt)
            * planet.rotation
            * Quat::from_rotation_z(delta / params.orbit.day_length * TAU);
        surface.last_tilt = params.orbit.obliquity;
    }
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
                .spawn((TransformBundle::default(), PlanetSurface::default()))
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
        app.insert_resource(OrbitRunning(false)).add_systems(
            Update,
            (|time: Res<Time<Virtual>>| time.delta_seconds())
                .pipe(update_planet_transforms)
                .run_if(resource_equals(OrbitRunning(true))),
        );
    }
}
