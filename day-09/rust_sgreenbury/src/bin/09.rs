use itertools::Itertools;

#[cfg(not(feature = "minimal"))]
advent_of_code::solution!(9);

#[cfg(feature = "minimal")]
fn main() {
    let input = std::fs::read_to_string(format!("data/inputs/{0:02}.txt", 9)).unwrap();
    println!("{}", part_one(&input).unwrap());
    println!("{}", part_two(&input).unwrap());
}

pub fn part_one(input: &str) -> Option<i32> {
    Some(input.lines().map(extrapolate).sum())
}

pub fn part_two(input: &str) -> Option<i32> {
    Some(input.lines().map(extrapolate_front).sum())
}

fn calculate_diffs(nums: &[i32]) -> Vec<i32> {
    nums.iter()
        .zip(nums.iter().skip(1))
        .map(|(x0, x1)| x1 - x0)
        .collect_vec()
}

fn calculate_all_diffs(line: &str) -> Vec<Vec<i32>> {
    let mut diffs = vec![line
        .split_whitespace()
        .map(|el| el.parse().unwrap())
        .collect_vec()];
    loop {
        diffs.push(calculate_diffs(diffs.last().unwrap()));
        if diffs.last().unwrap().iter().all(|el| *el == 0) {
            break diffs;
        }
    }
}

fn extrapolate(line: &str) -> i32 {
    calculate_all_diffs(line)
        .iter()
        .rev()
        .skip(1)
        .fold(0, |acc, el| acc + el.last().unwrap())
}

fn extrapolate_front(line: &str) -> i32 {
    calculate_all_diffs(line)
        .iter()
        .rev()
        .skip(1)
        .fold(0, |acc, el| el.first().unwrap() - acc)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result =
            part_one(&std::fs::read_to_string(format!("data/examples/{0:02}.txt", 9)).unwrap());
        assert_eq!(result, Some(114));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&std::fs::read_to_string(format!("data/examples/{0:02}.txt", 9)).unwrap());
        assert_eq!(result, Some(2));
    }
}
