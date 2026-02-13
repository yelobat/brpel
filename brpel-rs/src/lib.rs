//! BRP Scene Serialization API

use bevy::prelude::*;
#[cfg(not(target_arch = "wasm32"))]
use bevy::tasks::IoTaskPool;

/// Scene Save Request. On insertion, a save request is initiated.
#[derive(Debug, Resource, Reflect)]
#[reflect(Resource)]
#[type_path = "brpel::api"]
struct SaveRequest {
    pub path: String,
    pub with_components: Vec<String>,
    pub must_components: Vec<String>,
}

/// Save the current world to a scene file specified in the
/// SaveRequest path.
fn save_scene(world: &mut World) {
    if let Some(request) = world.get_resource::<SaveRequest>() {
        {
            let registry = world.resource::<AppTypeRegistry>().clone();
            let registry = registry.read();

            let mut filter = SceneFilter::deny_all();
            for type_path in request.with_components.iter() {
                if let Some(registration) = registry.get_with_type_path(type_path) {
                    filter = filter.allow_by_id(registration.type_id());
                } else {
                    warn!("Could not find type '{}' in registry", type_path);
                };
            }

            let must_have_ids: Vec<_> = request
                .must_components
                .iter()
                .filter_map(|type_path| registry.get_with_type_path(type_path))
                .map(|registry| registry.type_id())
                .collect();

            let request_path = request.path.clone();

            let filtered_entities = world
                .query::<EntityRef>()
                .iter(world)
                .filter(|entity_ref| {
                    must_have_ids
                        .iter()
                        .any(|id| entity_ref.contains_type_id(*id))
                })
                .map(|entity_ref| entity_ref.id())
                .collect::<Vec<_>>();

            let scene = DynamicSceneBuilder::from_world(world)
                .with_component_filter(filter)
                .extract_entities(filtered_entities.into_iter())
                .build();

            match scene.serialize(&registry) {
                Ok(serialized) => {
                    #[cfg(not(target_arch = "wasm32"))]
                    IoTaskPool::get()
                        .spawn(async move {
                            use std::{fs::File, io::Write};
                            File::create(request_path)
                                .and_then(|mut file| file.write(serialized.as_bytes()))
                                .expect("Error while writing scene to a file!");
                        })
                        .detach();
                }
                Err(e) => error!("BRP Save Error: Serialization failed: {}", e),
            }
        }

        world.remove_resource::<SaveRequest>();
    }
}

pub fn plugin(app: &mut App) {
    app.add_systems(Update, save_scene);
    app.register_type::<SaveRequest>();
}
