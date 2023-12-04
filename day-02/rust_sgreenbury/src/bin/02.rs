advent_of_code::solution!(2);

use anyhow::anyhow;
use hashbrown::HashMap;
use std::str::FromStr;

pub fn part_one(input: &str) -> Option<u32> {
    Some(
        input
            .lines()
            .enumerate()
            .filter_map(|(idx, line)| {
                if parse_line_part_1(line) {
                    Some(idx as u32 + 1)
                } else {
                    None
                }
            })
            .sum(),
    )
}

pub fn part_two(input: &str) -> Option<u32> {
    Some(input.lines().map(parse_line_part_2).sum())
}

#[derive(Hash, PartialEq, Eq)]
enum Colour {
    Red,
    Green,
    Blue,
}

impl FromStr for Colour {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "red" => Ok(Colour::Red),
            "green" => Ok(Colour::Green),
            "blue" => Ok(Colour::Blue),
            _ => Err(anyhow!(
                "Could not parse '{s}' as 'red', 'green' or 'blue'."
            )),
        }
    }
}

fn counts(line: &str) -> HashMap<Colour, usize> {
    let mut counts: HashMap<Colour, usize> = HashMap::new();
    let (_, games) = line.split_once(':').unwrap();
    games.trim().split(';').for_each(|game| {
        let game = game.trim();
        game.split(',').for_each(|item| {
            let (count, colour_str) = item.trim().split_once(' ').unwrap();
            let colour = colour_str.parse::<Colour>().unwrap();
            let count = count.parse::<usize>().unwrap();
            counts
                .entry(colour)
                .and_modify(|v| *v = count.max(*v))
                .or_insert(count);
        })
    });
    counts
}

fn parse_line_part_1(line: &str) -> bool {
    counts(line)
        .into_iter()
        .map(|(k, v)| match k {
            Colour::Red => v <= 12,
            Colour::Green => v <= 13,
            Colour::Blue => v <= 14,
        })
        .all(|b| b)
}

fn parse_line_part_2(line: &str) -> u32 {
    counts(line).into_values().map(|v| v as u32).product()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(8));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(2286));
    }
}
