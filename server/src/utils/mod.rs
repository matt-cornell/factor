use bevy::math::Vec2;
use rand::Rng;

pub mod database;
pub mod db_value;
pub mod mesh;
pub mod option_bytes;

pub fn random_point_in_quadrilateral<R: Rng + ?Sized>(points: [Vec2; 4], rng: &mut R) -> Vec2 {
    // Generate two uniformly distributed random numbers
    let r1 = rng.gen::<f32>();
    let r2 = rng.gen_range(0.0..=(1.0 - r1));

    // Compute the total area of the quadrilateral via vector cross products
    let area_triangle1 = 0.5
        * (points[1] - points[0])
            .perp_dot(points[2] - points[0])
            .abs();
    let area_triangle2 = 0.5
        * (points[2] - points[0])
            .perp_dot(points[3] - points[0])
            .abs();
    let total_area = area_triangle1 + area_triangle2;

    // Determine the intersection point to split triangles by area
    let split_factor = area_triangle1 / total_area;

    // Perform uniform point selection based on area-weighted triangulation
    if rng.gen_bool(split_factor as _) {
        // Select point in the second triangle
        let r1 = 1.0 - r1;
        let r2 = 1.0 - r2;

        points[0] + (points[2] - points[0]) * r2 + (points[3] - points[0]) * r1
    } else {
        // Select point in the first triangle
        points[0] + (points[1] - points[0]) * r1 + (points[2] - points[0]) * r2
    }
}
