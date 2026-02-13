//! Example usage of the SaveRequest functionality.

use std::net::Ipv4Addr;

use bevy::{
    prelude::*,
    render::{
        RenderPlugin,
        settings::{PowerPreference, RenderCreation, WgpuSettings},
    },
    scene::SceneInstanceReady,
};
use bevy_remote::{RemotePlugin, http::RemoteHttpPlugin};

/// The scene file to load.
const SCENE_FILE: &'static str = "scenes/test.scn.ron";

/// The title of the bevy window.
const TITLE: &'static str = "brpel-rs";

/// An exportable sprite which stores the color and custom_size
/// of the sprite.
///
/// NOTE: More information can be added to restore other aspects
/// of the sprite if necessary.
#[derive(Component, Reflect)]
#[reflect(Component)]
struct ExportableSprite {
    pub color: Color,
    pub custom_size: Option<Vec2>,
}

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins
                .set(WindowPlugin {
                    primary_window: Some(Window {
                        resolution: (1280, 720).into(),
                        title: TITLE.into(),
                        present_mode: bevy::window::PresentMode::AutoVsync,
                        ..default()
                    }),
                    ..default()
                })
                .set(RenderPlugin {
                    render_creation: RenderCreation::Automatic(WgpuSettings {
                        power_preference: PowerPreference::LowPower,
                        ..default()
                    }),
                    ..default()
                }),
            RemotePlugin::default(),
            RemoteHttpPlugin::default()
                .with_address(Ipv4Addr::new(127, 0, 0, 1))
                .with_port(3030),
            brpel_rs::plugin,
        ))
        .register_type::<ExportableSprite>()
        .add_systems(Startup, setup)
        .add_systems(Startup, load_scene.after(setup))
        .add_observer(handle_scene_load)
        .add_observer(insert_exportable_sprite)
        .add_observer(insert_sprite)
        .run();
}

/// When a Sprite is added, add the ExportableSprite
/// so scene serialization can retain the Sprite data.
fn insert_exportable_sprite(
    sprite_added: On<Add, Sprite>,
    sprites: Query<&mut Sprite>,
    mut commands: Commands,
) {
    let entity = sprite_added.entity;
    let Ok(sprite) = sprites.get(entity) else {
        warn!("Failed to get sprite for entity: {}", entity);
        return;
    };

    commands
        .entity(sprite_added.entity)
        .insert(ExportableSprite {
            color: sprite.color,
            custom_size: sprite.custom_size,
        });
}

/// When a ExportableSprite is added, restore the Sprite
/// back onto the Entity.
fn insert_sprite(
    sprite_added: On<Add, ExportableSprite>,
    sprites: Query<&mut ExportableSprite, Without<Sprite>>,
    mut commands: Commands,
) {
    let entity = sprite_added.entity;
    let Ok(sprite) = sprites.get(entity) else {
        warn!("Failed to get exportable sprite for entity: {}", entity);
        return;
    };

    commands.entity(entity).insert(Sprite::from_color(
        sprite.color,
        sprite.custom_size.unwrap(),
    ));
}

/// Load the scene from `SCENE_FILE`.
fn load_scene(mut commands: Commands, asset_server: Res<AssetServer>) {
    let scene = asset_server.load(SCENE_FILE);
    commands.spawn(DynamicSceneRoot(scene));
}

/// Function for handling additional logic when the scene
/// is loaded.
fn handle_scene_load(scene_ready: On<SceneInstanceReady>) {
    info!("Scene has finished loading: {:?}", scene_ready);
}

fn setup(mut commands: Commands) {
    commands.spawn(Camera2d);
    // This is the original scene saved to SCENE_FILE.
    commands.spawn((
        Name::new("Black Sprite"),
        Sprite::from_color(Color::Srgba(Srgba::BLACK), Vec2::new(400.0, 150.0)),
        Transform::from_translation((-300.0, -100.0, -1.0).into()),
    ));
    commands.spawn((
        Name::new("White Sprite"),
        Sprite::from_color(Color::Srgba(Srgba::WHITE), Vec2::new(400.0, 150.0)),
        Transform::from_translation((300.0, 100.0, -2.0).into()),
    ));
    commands.spawn((
        Name::new("Red Sprite"),
        Sprite::from_color(Color::Srgba(Srgba::RED), Vec2::new(100.0, 100.0)),
        Transform::from_translation((0.0, 0.0, 0.0).into()),
    ));
}
