#[macro_use]
extern crate log;
extern crate pretty_env_logger;

use std::collections::{HashSet, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader};

type MapType = [[char; MAP_SIZE]; MAP_SIZE];
const MAP_SIZE: usize = 32;
const EMPTY_SPACE: char = '.';

#[derive(Copy, Clone, Debug)]
struct Player {
    id: usize,
    x: usize,
    y: usize,
    dead: bool,
    attack: i16,
    hp: i16,
    player_type: char,
    enemy_type: char,
}

#[derive(Debug)]
struct PlayerVector {
    weight: usize,
    x: usize,
    y: usize,
}

fn has_enemy_neighbor(player: &Player, players: &Vec<Player>, map: &MapType) -> Option<usize> {
    let mut hps = [0i16; 4];
    let mut min_hp = std::i16::MAX;
    let mut indexes = [0usize; 4];
    if map[player.x][player.y - 1] == player.enemy_type {
        let (i, target) = players
            .iter()
            .enumerate()
            .find(|p| !p.1.dead && p.1.x == player.x && p.1.y == player.y - 1)
            .expect("could not find the target north");
        hps[0] = target.hp;
        indexes[0] = i;
        if target.hp < min_hp {
            min_hp = target.hp;
        }
    }
    if map[player.x - 1][player.y] == player.enemy_type {
        let (i, target) = players
            .iter()
            .enumerate()
            .find(|p| !p.1.dead && p.1.x == player.x - 1 && p.1.y == player.y)
            .expect("could not find the target west");
        hps[1] = target.hp;
        indexes[1] = i;
        if target.hp < min_hp {
            min_hp = target.hp;
        }
    }
    if map[player.x + 1][player.y] == player.enemy_type {
        let (i, target) = players
            .iter()
            .enumerate()
            .find(|p| !p.1.dead && p.1.x == player.x + 1 && p.1.y == player.y)
            .expect("could not find the target east");
        hps[2] = target.hp;
        indexes[2] = i;
        if target.hp < min_hp {
            min_hp = target.hp;
        }
    }
    if map[player.x][player.y + 1] == player.enemy_type {
        let (i, target) = players
            .iter()
            .enumerate()
            .find(|p| !p.1.dead && p.1.x == player.x && p.1.y == player.y + 1)
            .expect("could not find the target south");
        hps[3] = target.hp;
        indexes[3] = i;
        if target.hp < min_hp {
            min_hp = target.hp;
        }
    }

    if hps[0] == min_hp {
        Some(indexes[0])
    } else if hps[1] == min_hp {
        Some(indexes[1])
    } else if hps[2] == min_hp {
        Some(indexes[2])
    } else if hps[3] == min_hp {
        Some(indexes[3])
    } else {
        None
    }
}

fn find_next_move(player: &Player, map: &MapType) -> Option<PlayerVector> {
    match find_enemy(player, map) {
        Some(target) => {
            debug!("found target {:?}", target);
            let best_direction = find_best_direction_to_player(player, map, &target);
            debug!("best direction {:?}", best_direction);
            best_direction
        }
        None => None,
    }
}

fn find_enemy(player: &Player, map: &MapType) -> Option<PlayerVector> {
    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back(PlayerVector {
        x: player.x,
        y: player.y,
        weight: 0,
    });
    while let Some(coord) = queue.pop_front() {
        if !visited.insert((coord.x, coord.y)) {
            continue;
        }
        if map[coord.x][coord.y] == player.enemy_type {
            return Some(coord);
        }
        if coord.weight > 0 {
            match map[coord.x][coord.y] {
                'E' => continue,
                'G' => continue,
                '#' => continue,
                _ => ()
            }
        }
        queue.push_back(PlayerVector {
            x: coord.x,
            y: coord.y - 1,
            weight: coord.weight + 1,
        });
        queue.push_back(PlayerVector {
            x: coord.x - 1,
            y: coord.y,
            weight: coord.weight + 1,
        });
        queue.push_back(PlayerVector {
            x: coord.x + 1,
            y: coord.y,
            weight: coord.weight + 1,
        });
        queue.push_back(PlayerVector {
            x: coord.x,
            y: coord.y + 1,
            weight: coord.weight + 1,
        });
    }
    None
}

fn find_best_direction_to_player(
    player: &Player,
    map: &MapType,
    source: &PlayerVector,
) -> Option<PlayerVector> {
    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back(PlayerVector {
        x: source.x,
        y: source.y,
        weight: 0,
    });
    visited.insert((source.x, source.y));
    let mut destinations = HashSet::new();
    destinations.insert((player.x, player.y - 1));
    destinations.insert((player.x - 1, player.y));
    destinations.insert((player.x + 1, player.y));
    destinations.insert((player.x, player.y + 1));
    while let Some(coord) = queue.pop_front() {
        if coord.weight > 0 {
            if !visited.insert((coord.x, coord.y)) {
                continue;
            }
            if map[coord.x][coord.y] != EMPTY_SPACE {
                continue;
            }
        }
        // it's weight - 1 because we are only travelling to the neighbor cell
        if coord.weight == source.weight - 1 && destinations.contains(&(coord.x, coord.y)) {
            return Some(coord);
        }
        queue.push_back(PlayerVector {
            x: coord.x,
            y: coord.y - 1,
            weight: coord.weight + 1,
        });
        queue.push_back(PlayerVector {
            x: coord.x - 1,
            y: coord.y,
            weight: coord.weight + 1,
        });
        queue.push_back(PlayerVector {
            x: coord.x + 1,
            y: coord.y,
            weight: coord.weight + 1,
        });
        queue.push_back(PlayerVector {
            x: coord.x,
            y: coord.y + 1,
            weight: coord.weight + 1,
        });
    }
    None
}

fn play_game(mut players: Vec<Player>, map: &mut MapType) -> (i32, usize) {
    let mut rounds = 0;
    let player_count = players.len();
    loop {
        info!("round {}", rounds);
        for i in 0..MAP_SIZE {
            let mut row = String::new();
            for j in 0..MAP_SIZE {
                row.push(map[j][i]);
            }
            debug!("{}", row);
        }
        players.sort_by(|a, b| {
            if a.y == b.y {
                a.x.cmp(&b.x)
            } else {
                a.y.cmp(&b.y)
            }
        });

        let mut quit_condition = false;
        for i in 0..player_count {
            let mut player = players[i];
            if player.dead {
                continue;
            }
            debug!("{:?}", player);

            if !players
                .iter()
                .any(|p| !p.dead && p.enemy_type == player.player_type)
            {
                if i < player_count - 1 {
                    debug!("quitting before end of round");
                    quit_condition = true;
                }
                break;
            }

            match has_enemy_neighbor(&player, &players, map) {
                Some(_) => {
                    debug!("have enemy neighbor, no moving allowed");
                    ()
                }
                None => match find_next_move(&player, map) {
                    Some(new_vect) => {
                        debug!("moving");
                        map[player.x][player.y] = EMPTY_SPACE;
                        players[i].x = new_vect.x;
                        players[i].y = new_vect.y;
                        player.x = new_vect.x;
                        player.y = new_vect.y;
                        map[new_vect.x][new_vect.y] = player.player_type;
                    }
                    None => (),
                },
            }
            match has_enemy_neighbor(&player, &players, map) {
                Some(enemy_index) => {
                    debug!("attacking {:?}", players[enemy_index]);
                    players[enemy_index].hp -= player.attack;
                    if players[enemy_index].hp <= 0 {
                        debug!("enemy is dead");
                        players[enemy_index].dead = true;
                        players[enemy_index].hp = 0;
                        map[players[enemy_index].x][players[enemy_index].y] = EMPTY_SPACE;
                    }
                }
                None => {
                    debug!("no enemy neighbors, ending turn");
                    ()
                }
            }
        }

        if quit_condition {
            debug!("quitting condition");
            break;
        }

        rounds += 1;
    }

    let sum: i32 = players.iter().map(|p| p.hp as i32).sum();
    info!("sum {} rounds {}", sum, rounds);
    let elf_count = players.iter().filter(|p| p.player_type == 'E' && !p.dead).count();
    (sum * rounds, elf_count)
}

fn main() {
    pretty_env_logger::init();
    let lines: Vec<String> = BufReader::new(File::open("input").unwrap())
        .lines()
        .map(|line| line.unwrap())
        .collect();
    let mut map = [[EMPTY_SPACE; MAP_SIZE]; MAP_SIZE];
    let mut y = 0;
    let mut players: Vec<Player> = Vec::with_capacity(20);
    let mut id = 0;
    let mut elf_count = 0;
    for line in lines.iter() {
        for (x, c) in line.char_indices() {
            map[x][y] = c;
            if c == '.' {
                map[x][y] = EMPTY_SPACE;
            }
            if c != 'E' && c != 'G' {
                continue;
            }
            if c == 'E' {
                elf_count += 1;
            }
            let enemy_type = if c == 'E' { 'G' } else { 'E' };
            id += 1;
            players.push(Player {
                id,
                x,
                y,
                dead: false,
                attack: 3,
                hp: 200,
                player_type: c,
                enemy_type,
            });
        }
        y += 1;
    }
    let (score, _elves_remaining) = play_game(players.clone(), &mut map.clone());
    println!("score of winning team is {}", score);
    for i in 4..50 {
        let new_players: Vec<Player> = players.clone().into_iter().map(|p| {
            if p.player_type == 'G' {
                p
            } else {
                Player {
                    id: p.id,
                    x: p.x,
                    y: p.y,
                    dead: false,
                    attack: i,
                    hp: 200,
                    player_type: 'E',
                    enemy_type: 'G'
                }
            }
        }).collect();
        let (score, elves_remaining) = play_game(new_players, &mut map.clone());
        if elves_remaining == elf_count {
            println!("all elves survived with a score of {} with attack of {}", score, i);
            break;
        }
    }
}
