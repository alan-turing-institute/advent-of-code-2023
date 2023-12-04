use anyhow::anyhow;
use hashbrown::HashMap;
use itertools::Itertools;
pub use nom::character::complete::{alpha0, digit0};
use nom::{bytes::complete::take_while, IResult};
use std::str::FromStr;

advent_of_code::solution!(3);

pub fn part_one(input: &str) -> Option<u32> {
    Some(
        Engine::parse(input)
            .numbers_with_adjacent_symbols()
            .iter()
            .sum(),
    )
}

pub fn part_two(input: &str) -> Option<u32> {
    Some(Engine::parse(input).gear_ratios().iter().sum())
}

#[derive(Debug)]
struct Engine {
    pub symbols: HashMap<Point, Symbol>,
    pub numbers: Vec<Number>,
    pub dim: (usize, usize),
}

impl Engine {
    fn parse(input: &str) -> Self {
        let mut numbers: Vec<Number> = vec![];
        let mut symbols: HashMap<Point, Symbol> = HashMap::new();
        let dim = (input.lines().next().unwrap().len(), input.lines().count());
        input.lines().enumerate().for_each(|(y, mut line)| {
            while !line.is_empty() {
                match line.chars().next().unwrap() {
                    c if c.is_ascii_digit() => {
                        let (remaining, number) = parser_digits(line).unwrap();
                        let length = number.len();
                        let number = number.parse::<u32>().unwrap();
                        line = remaining;
                        let number = Number {
                            number,
                            length,
                            loc: Point {
                                x: dim.0 - length - line.len(),
                                y,
                            },
                        };
                        numbers.push(number);
                    }
                    '.' => {
                        let (remaining, _) = parser_periods(line).unwrap();
                        line = remaining;
                    }
                    _ => {
                        let symbol: Symbol = line[..1].parse().unwrap();
                        symbols.insert(
                            Point {
                                x: dim.0 - line.len(),
                                y,
                            },
                            symbol,
                        );
                        line = &line[1..];
                    }
                }
            }
        });
        Self {
            symbols,
            numbers,
            dim,
        }
    }
    fn numbers_with_adjacent_symbols(&self) -> Vec<u32> {
        self.numbers.iter().fold(Vec::new(), |mut acc, number| {
            let points = number.adjacent(self.dim);
            if points.iter().any(|point| self.symbols.contains_key(point)) {
                acc.push(number.number)
            }
            acc
        })
    }
    fn gear_ratios(&self) -> Vec<u32> {
        self.numbers
            .iter()
            .fold(HashMap::<Point, Vec<u32>>::new(), |mut acc, number| {
                let points = number.adjacent(self.dim);
                points.iter().for_each(|point| {
                    if let Some(Symbol::Star) = self.symbols.get(point) {
                        acc.entry(*point)
                            .and_modify(|v| v.push(number.number))
                            .or_insert(vec![number.number]);
                    }
                });
                acc
            })
            .into_iter()
            .filter_map(|(_, v)| {
                if v.len() == 2 {
                    Some(v.iter().product())
                } else {
                    None
                }
            })
            .collect_vec()
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct Point {
    x: usize,
    y: usize,
}

fn parser_digits(input: &str) -> IResult<&str, &str> {
    digit0(input)
}

fn parser_periods(input: &str) -> IResult<&str, &str> {
    take_while(|c| c == '.')(input)
}

impl FromStr for Symbol {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Self::Plus),
            "*" => Ok(Self::Star),
            "#" => Ok(Self::Hash),
            "/" => Ok(Self::Slash),
            "&" => Ok(Self::Amp),
            "-" => Ok(Self::Dash),
            "%" => Ok(Self::Percent),
            "=" => Ok(Self::Equals),
            "$" => Ok(Self::Dollar),
            "@" => Ok(Self::At),
            _ => Err(anyhow!("'{s}' is not a valid symbol.")),
        }
    }
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Symbol {
    Plus,
    Star,
    Hash,
    Slash,
    Amp,
    Dash,
    Percent,
    Equals,
    Dollar,
    At,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Number {
    number: u32,
    length: usize,
    loc: Point,
}

impl Number {
    fn adjacent(&self, dim: (usize, usize)) -> Vec<Point> {
        let mut points: Vec<Point> = vec![];
        let (y_low, y_high) = (self.loc.y.max(1) - 1, (self.loc.y + 1).min(dim.1 - 1));
        let (x_low, x_high) = (
            self.loc.x.max(1) - 1,
            (self.loc.x + self.length).min(dim.0 - 1),
        );
        for y in y_low..=y_high {
            for x in x_low..=x_high {
                if y == self.loc.y && x >= self.loc.x && x < self.loc.x + self.length {
                    continue;
                }
                points.push(Point { x, y });
            }
        }
        points
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(4361));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(467835));
    }
}
