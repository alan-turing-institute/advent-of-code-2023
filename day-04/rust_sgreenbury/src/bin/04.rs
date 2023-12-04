use hashbrown::{HashMap, HashSet};
use itertools::Itertools;
use std::str::FromStr;

#[cfg(not(feature = "minimal"))]
advent_of_code::solution!(4);

#[cfg(feature = "minimal")]
fn main() {
    let input = std::fs::read_to_string(format!("data/inputs/{0:02}.txt", 4)).unwrap();
    println!("{}", part_one(&input).unwrap());
    println!("{}", part_two(&input).unwrap());
}

pub fn part_one(input: &str) -> Option<u32> {
    Some(
        input
            .lines()
            .map(|line| line.parse::<Card>().unwrap().score())
            .sum(),
    )
}

pub fn part_two(input: &str) -> Option<u32> {
    Some(input.parse::<Cards>().unwrap().counts.values().sum())
}

impl FromStr for Cards {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            counts: s
                .lines()
                .map(|line| line.parse::<Card>().unwrap())
                .collect_vec()
                .iter()
                .fold(HashMap::<usize, u32>::new(), |mut acc, card| {
                    let num_of_cards_id = *acc.entry(card.id).and_modify(|v| *v += 1).or_insert(1);
                    (1..=card.count()).for_each(|idx| {
                        acc.entry(card.id + idx)
                            .and_modify(|v| *v += num_of_cards_id)
                            .or_insert(num_of_cards_id);
                    });
                    acc
                }),
        })
    }
}

#[derive(Debug)]
struct Cards {
    counts: HashMap<usize, u32>,
}

#[derive(Debug)]
struct Card {
    id: usize,
    winning_numbers: HashSet<u32>,
    your_numbers: HashSet<u32>,
}

impl FromStr for Card {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (id, numbers) = s.split_once(':').unwrap();
        let id = id
            .split_whitespace()
            .nth(1)
            .unwrap()
            .trim()
            .parse::<usize>()
            .unwrap();

        let (winning_numbers, your_numbers) = numbers.trim().split_once('|').unwrap();
        Ok(Self {
            id,
            winning_numbers: winning_numbers
                .split_whitespace()
                .map(|num| num.trim().parse::<u32>().unwrap())
                .collect::<HashSet<_>>(),
            your_numbers: your_numbers
                .split_whitespace()
                .map(|num| num.trim().parse::<u32>().unwrap())
                .collect::<HashSet<_>>(),
        })
    }
}

impl Card {
    fn count(&self) -> usize {
        self.your_numbers
            .iter()
            .filter(|num| self.winning_numbers.contains(*num))
            .count()
    }
    fn score(&self) -> u32 {
        let count = self.count();
        if count > 0 {
            2u32.pow(count as u32 - 1)
        } else {
            0
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result =
            part_one(&std::fs::read_to_string(format!("data/examples/{0:02}.txt", 4)).unwrap());
        assert_eq!(result, Some(13));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&std::fs::read_to_string(format!("data/examples/{0:02}.txt", 4)).unwrap());
        assert_eq!(result, Some(30));
    }
}
