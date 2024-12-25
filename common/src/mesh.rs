use bevy::asset::RenderAssetUsages;
use bevy::prelude::*;
use bevy::render::mesh::{Indices, PrimitiveTopology};
use serde::{Deserialize, Serialize};

/// A mesh, but easier to use than Bevy's `Mesh`.
#[derive(Debug, Default, Clone, Serialize, Deserialize, Component)]
pub struct MeshData {
    pub vertices: Vec<Vec3>,
    pub triangles: Vec<[u32; 3]>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub uv: Vec<Vec2>,
}
impl MeshData {
    pub fn build_bevy(self) -> Mesh {
        let mut out = Mesh::new(
            PrimitiveTopology::TriangleList,
            RenderAssetUsages::RENDER_WORLD,
        )
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, self.vertices)
        .with_inserted_indices(Indices::U32(bytemuck::cast_vec(self.triangles)));
        if !self.uv.is_empty() {
            out.insert_attribute(Mesh::ATTRIBUTE_UV_0, self.uv);
        }
        out
    }
}
impl From<MeshData> for Mesh {
    fn from(value: MeshData) -> Self {
        value.build_bevy()
    }
}
