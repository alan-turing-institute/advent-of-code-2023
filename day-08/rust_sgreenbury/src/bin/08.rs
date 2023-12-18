use anyhow::anyhow;
use hashbrown::HashMap;
use itertools::Itertools;
use num::integer::lcm;
use regex::Regex;
use std::{cell::RefCell, collections::VecDeque, rc::Rc};

#[cfg(not(feature = "minimal"))]
advent_of_code::solution!(8);

#[cfg(feature = "minimal")]
fn main() {
    let input = std::fs::read_to_string(format!("data/inputs/{0:02}.txt", 8)).unwrap();
    println!("{}", part_one(&input).unwrap());
    println!("{}", part_two(&input).unwrap());
}

enum Part {
    One,
    Two,
}

fn solve(input: &str, part: &Part) -> Option<u32> {
    let mut graph = Graph::new(input, part);
    let mut cycle = graph.directions.iter().cycle().enumerate();
    loop {
        let mut next_nodes = VecDeque::new();
        if let Some((_, direction)) = cycle.next() {
            while let Some(node) = graph.current.pop_front() {
                let next = match direction {
                    Direction::Left => node.borrow().left.as_ref().unwrap().clone(),
                    Direction::Right => node.borrow().right.as_ref().unwrap().clone(),
                };
                next_nodes.push_back(next);
            }
            graph.current = next_nodes;
        }

        match part {
            Part::One => {
                if graph.current.front().unwrap().borrow().label == "ZZZ" {
                    break;
                }
            }
            Part::Two => {
                if graph
                    .current
                    .iter()
                    .all(|node| node.borrow().label.ends_with('Z'))
                {
                    break;
                }
            }
        }
    }
    Some(cycle.next().unwrap().0 as u32)
}

pub fn part_one(input: &str) -> Option<u32> {
    solve(input, &Part::One)
}

pub fn part_two(input: &str) -> Option<usize> {
    // Too long!
    // solve(input, &Part::Two)

    // Measure the period of each last path's last Z and get LCM
    let graph = Graph::new(input, &Part::Two);
    Some(graph.path_lcm())
}

#[derive(Debug)]
struct Node {
    label: String,
    left: Option<Rc<RefCell<Node>>>,
    right: Option<Rc<RefCell<Node>>>,
}

impl TryFrom<char> for Direction {
    type Error = anyhow::Error;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            'L' => Ok(Self::Left),
            'R' => Ok(Self::Right),
            _ => Err(anyhow!("Not a valid direction: {c}")),
        }
    }
}

enum Direction {
    Left,
    Right,
}

struct Graph {
    current: VecDeque<Rc<RefCell<Node>>>,
    directions: Vec<Direction>,
}

impl Graph {
    fn new(input: &str, part: &Part) -> Self {
        let (directions, graph) = input.split_once("\n\n").unwrap();
        let directions = directions
            .chars()
            .flat_map(Direction::try_from)
            .collect_vec();

        let re = Regex::new(r"([A-Z|0-9]{3}) = \(([A-Z|0-9]{3}), ([A-Z|0-9]{3})\)").unwrap();
        let mut hm: HashMap<String, _> = HashMap::new();
        let captures = re.captures_iter(graph).collect_vec();
        captures.iter().for_each(|capture| {
            let (_, [label, _, _]) = capture.extract();
            let node = Rc::new(RefCell::new(Node {
                label: label.to_string(),
                left: None,
                right: None,
            }));
            hm.insert(label.to_string(), node);
        });
        captures.iter().for_each(|capture| {
            let (_, [label, left, right]) = capture.extract();
            let mut node = hm.get(label).unwrap().borrow_mut();
            let left = hm.get(left).unwrap();
            let right = hm.get(right).unwrap();
            node.left = Some(left.clone());
            node.right = Some(right.clone());
        });

        match part {
            Part::One => Graph {
                current: VecDeque::from([hm.get("AAA").unwrap().clone()]),
                directions,
            },
            Part::Two => Graph {
                current: hm
                    .into_iter()
                    .filter_map(|(k, v)| {
                        if k.ends_with('A') {
                            Some(v.clone())
                        } else {
                            None
                        }
                    })
                    .collect(),
                directions,
            },
        }
    }

    fn calculate_period(&self, start_node: Rc<RefCell<Node>>) -> usize {
        let mut it = self.directions.iter().cycle().enumerate();
        let mut current_node = start_node.clone();
        let mut last_z_visits = vec![];
        let period = loop {
            let (idx, next_direction) = it.next().unwrap();
            let next_node = match next_direction {
                Direction::Left => current_node.borrow().left.as_ref().unwrap().clone(),
                Direction::Right => current_node.borrow().right.as_ref().unwrap().clone(),
            };
            if current_node.borrow().label.ends_with('Z') {
                last_z_visits.push(idx);
                let diffs = last_z_visits
                    .iter()
                    .zip(last_z_visits.iter().skip(1))
                    .map(|(l0, l1)| l1 - l0)
                    .rev()
                    .collect_vec();
                if diffs.iter().unique().collect_vec().len() == 1 {
                    break *diffs.first().unwrap();
                }
            }
            current_node = next_node;
        };
        period
    }

    fn path_periods(&self) -> Vec<usize> {
        self.current
            .iter()
            .map(|node| self.calculate_period(node.clone()))
            .collect_vec()
    }

    fn path_lcm(&self) -> usize {
        self.path_periods().into_iter().fold(1, lcm)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result =
            part_one(&std::fs::read_to_string(format!("data/examples/{0:02}.txt", 8)).unwrap());
        assert_eq!(result, Some(2));
    }

    const PART_TWO: &str = r#"LR

    11A = (11B, XXX)
    11B = (XXX, 11Z)
    11Z = (11B, XXX)
    22A = (22B, XXX)
    22B = (22C, 22C)
    22C = (22Z, 22Z)
    22Z = (22B, 22B)
    XXX = (XXX, XXX)
    "#;

    #[test]
    fn test_part_two() {
        let result = part_two(PART_TWO);
        assert_eq!(result, Some(6));
    }

    #[test]
    fn test_period_and_lcm() {
        let graph = Graph::new(PART_TWO, &Part::Two);
        let periods = graph.path_periods();
        assert_eq!(vec![2, 3], periods);
        let lcm = graph.path_lcm();
        assert_eq!(6, lcm);
    }
}
