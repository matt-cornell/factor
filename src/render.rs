use crate::state::SingleplayerState;
use bevy::prelude::*;
use bevy::utils::hashbrown::HashMap;
use bevy_egui::{egui, EguiContexts};
use factor_client::core_ui::ClientState;
use factor_server::config::WorldConfig;
use factor_server::storage::PersistentBackend;
use factor_server::utils::database::{Database, DatabaseError};
use std::time::Duration;

pub fn render_select_sp(
    mut commands: Commands,
    mut contexts: EguiContexts,
    mut next_state: ResMut<NextState<SingleplayerState>>,
    mut available: Local<Vec<(String, Option<DatabaseError>)>>,
    mut search: Local<String>,
    time: Res<Time>,
    mut last_run: Local<Option<Duration>>,
    mut needs_tick: Local<bool>,
) {
    let tick = if let Some(last_run) = &mut *last_run {
        time.elapsed() - *last_run > Duration::from_secs(5) && {
            *last_run = time.elapsed();
            true
        }
    } else {
        *last_run = Some(time.elapsed());
        true
    };
    if tick || *needs_tick {
        *needs_tick = false;
        let mut old = available
            .drain(..)
            .filter_map(|(a, b)| b.map(|b| (a, b)))
            .collect::<HashMap<_, _>>();
        available.clear();
        available.extend(PersistentBackend::list_saves().map(|name| {
            let err = old.remove(&name);
            (name, err)
        }));
        debug!(num = available.len(), "Found worlds");
    }
    egui::Area::new(egui::Id::new("Select singleplayer"))
        .anchor(egui::Align2::CENTER_CENTER, egui::Vec2::ZERO)
        .show(contexts.ctx_mut(), |ui| {
            ui.set_width(500.0);
            if ui.input(|input| input.key_pressed(egui::Key::Escape)) {
                next_state.set(SingleplayerState::Base(ClientState::MainMenu));
            }
            ui.with_layout(egui::Layout::top_down(egui::Align::Center), |ui| {
                ui.heading("Singleplayer");
            });

            ui.with_layout(egui::Layout::left_to_right(egui::Align::Min).with_main_justify(true), |ui| {
                ui.add(egui::TextEdit::singleline(&mut *search).desired_width(f32::INFINITY));
                if ui.button("New World").clicked() {
                    next_state.set(SingleplayerState::WorldCreation(Some(std::mem::take(&mut *search))));
                }
            });

            {
                let mut prep = egui::Frame::group(ui.style()).begin(ui);
                {
                    let ui = &mut prep.content_ui;
                    ui.set_width(500.0);
                    ui.label(egui::RichText::new("Temporary World").size(14.0));
                    ui.label("This world will no longer exist after you close it. For testing/demo purposes only!");
                }
                if prep.allocate_space(ui).hovered() {
                    prep.frame.stroke.color = egui::Color32::from_gray(240);
                }
                if prep.end(ui).interact(egui::Sense::click()).clicked() {
                    info!("Creating temporary world");
                    next_state.set(SingleplayerState::WorldCreation(None));
                }
            }

            egui::ScrollArea::vertical().show(ui, |ui| {
                let mut any = false;
                for (world, erred) in &mut available {
                    let Some(idx) = world.find(&*search) else {
                        continue;
                    };
                    any = true;
                    let mut prep = egui::Frame::group(ui.style()).begin(ui);
                    {
                        let ui = &mut prep.content_ui;
                        ui.set_width(500.0);
                        let mut layout = egui::text::LayoutJob::single_section(
                            world[..idx].to_string(),
                            egui::TextFormat {
                                font_id: egui::FontId::proportional(14.0),
                                ..default()
                            },
                        );
                        layout.append(
                            &world[idx..(idx + search.len())],
                            0.0,
                            egui::TextFormat {
                                font_id: egui::FontId::proportional(14.0),
                                background: egui::Color32::YELLOW,
                                ..default()
                            },
                        );
                        layout.append(
                            &world[(idx + search.len())..],
                            0.0,
                            egui::TextFormat {
                                font_id: egui::FontId::proportional(14.0),
                                ..default()
                            },
                        );
                        ui.label(layout);
                        
                        let popup_id = egui::Id::new(("close-popup", &world));
                        
                        let delete_res = ui.with_layout(egui::Layout::right_to_left(egui::Align::Min), |ui| {
                            let res = ui.button("Delete");
                            if res.clicked() {
                                ui.memory_mut(|mem| mem.open_popup(popup_id));
                            }
                            res
                        });
                        egui::popup_above_or_below_widget(ui, popup_id, &delete_res.inner, egui::AboveOrBelow::Below, egui::PopupCloseBehavior::CloseOnClickOutside, |ui| {
                            ui.label(format!("Delete world {world:?}?"));
                            if ui.button("Confirm").clicked() {
                                PersistentBackend::delete_save(world);
                                ui.memory_mut(|mem| mem.close_popup());
                            }
                            if ui.button("Cancel").clicked() {
                                ui.memory_mut(|mem| mem.close_popup());
                            }
                        });

                        if let Some(err) = erred {
                            prep.frame.fill = if ui.style().visuals.dark_mode {
                                egui::Color32::DARK_RED
                            } else {
                                egui::Color32::RED
                            };
                            ui.label(format!("Error: {err}"));
                        }
                    }
                    if prep.allocate_space(ui).hovered() {
                        prep.frame.stroke.color =
                            egui::Color32::from_gray(if ui.style().visuals.dark_mode {
                                240
                            } else {
                                16
                            });
                    }
                    if prep.end(ui).interact(egui::Sense::click()).clicked() {
                        info!(name = world, "Loading world");
                        match Database::persistent(world.clone()) {
                            Ok(db) => {
                                commands.insert_resource(db);
                                next_state.set(SingleplayerState::Base(ClientState::WorldLoading));
                            }
                            Err(err) => {
                                *erred = Some(err);
                            }
                        }
                    }
                }

                if !any {
                    ui.label("No worlds available!");
                }
            });
        });
}

pub fn render_create_world(
    mut commands: Commands,
    mut contexts: EguiContexts,
    state: Res<State<SingleplayerState>>,
    mut next_state: ResMut<NextState<SingleplayerState>>,
    mut name_edit: Local<Option<String>>,
    mut seed_edit: Local<String>,
    mut config_wip: Local<Option<WorldConfig>>,
) {
    egui::Area::new(egui::Id::new("Create world"))
        .anchor(egui::Align2::CENTER_CENTER, egui::Vec2::ZERO)
        .show(contexts.ctx_mut(), |ui| {
            ui.heading("New World");

            if ui.input(|input| input.key_pressed(egui::Key::Escape)) {
                next_state.set(SingleplayerState::Base(ClientState::MainMenu));
            }

            let SingleplayerState::WorldCreation(name) = &**state else {
                unreachable!("render_create_world called in wrong state!")
            };

            let mut valid = if let Some(name) = name {
                ui.label("World Name");
                let name = name_edit.get_or_insert_with(|| name.clone());
                ui.text_edit_singleline(name);
                if !name
                    .chars()
                    .all(|c| c.is_alphanumeric() || c == ' ' || c.is_ascii_punctuation())
                {
                    ui.label(
                        egui::RichText::new("Invalid cahracter in name")
                            .background_color(ui.style().visuals.error_fg_color),
                    );
                    false
                } else if PersistentBackend::list_saves().any(|save| save == *name) {
                    ui.label(
                        egui::RichText::new("A save with this name already exists")
                            .background_color(ui.style().visuals.error_fg_color),
                    );
                    false
                } else {
                    true
                }
            } else {
                ui.label("Temporary World");
                true
            };

            ui.label("Random seed");
            ui.add(
                egui::TextEdit::singleline(&mut *seed_edit)
                    .char_limit(64)
                    .hint_text("a 64-character hex string"),
            );
            if !seed_edit.chars().all(|c| c.is_ascii_hexdigit()) {
                ui.label(
                    egui::RichText::new("Non-hex character in seed")
                        .background_color(ui.style().visuals.error_fg_color),
                );
                valid = false;
            }

            let config = config_wip.get_or_insert_default();

            if ui.add_enabled(valid, egui::Button::new("Create")).clicked() {
                let db = if name.is_some() {
                    Database::persistent(name_edit.take().unwrap()).unwrap()
                } else {
                    Database::temporary()
                };
                commands.insert_resource(db);

                if !seed_edit.is_empty() {
                    let remaining = 64 - seed_edit.len();
                    seed_edit.reserve(remaining);
                    seed_edit.extend(std::iter::repeat_n('0', remaining));
                    let mut buf = [0; 32];
                    hex::decode_to_slice(&*seed_edit, &mut buf)
                        .expect("We already validate the characters and length");
                    config.seed = Some(buf);
                }

                commands.insert_resource(config_wip.take().unwrap());

                next_state.set(SingleplayerState::Base(ClientState::WorldLoading));
            }
            if ui.button("Cancel").clicked() {
                seed_edit.clear();
                *name_edit = None;
                *config_wip = None;
                next_state.set(SingleplayerState::Base(ClientState::SPSelect));
            }
        });
}