use anyhow::{anyhow, Result};
use hashbrown::HashMap;

#[cfg(not(feature = "minimal"))]
advent_of_code::solution!(1);

#[cfg(feature = "minimal")]
fn main() {
    let input = std::fs::read_to_string(format!("data/inputs/{0:02}.txt", 1)).unwrap();
    println!("{}", part_one(&input).unwrap());
    println!("{}", part_two(&input).unwrap());
}

pub fn part_one(input: &str) -> Option<u32> {
    solve(input, parse_line_part_1)
}

pub fn part_two(input: &str) -> Option<u32> {
    solve(input, parse_line_part_2)
}

pub fn solve<F>(input: &str, parse_line: F) -> Option<u32>
where
    F: Fn(&str) -> anyhow::Result<u32>,
{
    Some(
        input
            .lines()
            .filter_map(|line| {
                let line = line.trim();
                if !line.is_empty() {
                    Some(parse_line(line))
                } else {
                    None
                }
            })
            .collect::<Result<Vec<u32>, _>>()
            .ok()?
            .into_iter()
            .sum(),
    )
}

lazy_static::lazy_static! {
    static ref DIGITS: HashMap<&'static str, &'static str> = {
        let mut digits = HashMap::new();
        digits.insert("one", "1");
        digits.insert("two", "2");
        digits.insert("three", "3");
        digits.insert("four", "4");
        digits.insert("five", "5");
        digits.insert("six", "6");
        digits.insert("seven", "7");
        digits.insert("eight", "8");
        digits.insert("nine", "9");
        digits.insert("1", "1");
        digits.insert("2", "2");
        digits.insert("3", "3");
        digits.insert("4", "4");
        digits.insert("5", "5");
        digits.insert("6", "6");
        digits.insert("7", "7");
        digits.insert("8", "8");
        digits.insert("9", "9");
        digits
    };
}
fn parse_line_part_1(line: &str) -> anyhow::Result<u32> {
    let mut digits = line.chars().filter(|c| c.is_ascii_digit());
    let first = digits.next().unwrap();
    Ok(format!("{}{}", first, &digits.next_back().unwrap_or(first)).parse::<u32>()?)
}

fn parse_line_part_2(line: &str) -> anyhow::Result<u32> {
    let first = get_digit(line, true).ok_or(anyhow!("No first digit."))?;
    let last = get_digit(line, false).ok_or(anyhow!("No last digit."))?;
    Ok(format!("{first}{last}").parse()?)
}

fn get_digit(line: &str, first: bool) -> Option<&str> {
    for idx in 0..line.len() {
        let substring = if first {
            &line[idx..]
        } else {
            &line[line.len() - 1 - idx..]
        };
        for (k, v) in DIGITS.iter() {
            if substring.starts_with(k) {
                return Some(v);
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result =
            part_one(&std::fs::read_to_string(format!("data/examples/{0:02}.txt", 1)).unwrap());
        assert_eq!(result, Some(142));
    }

    const PART_TWO: &str = r"two1nine
        eightwothree
        abcone2threexyz
        xtwone3four
        4nineeightseven2
        zoneight234
        7pqrstsixteen
    ";

    #[test]
    fn test_part_two() {
        let result = part_two(PART_TWO);

        assert_eq!(result, Some(281));
    }
}
