use itertools::Itertools;

type Int = u64;
type Float = f64;

#[cfg(not(feature = "minimal"))]
advent_of_code::solution!(6);

#[cfg(feature = "minimal")]
fn main() {
    let input = std::fs::read_to_string(format!("data/inputs/{0:02}.txt", 6)).unwrap();
    println!("{}", part_one(&input).unwrap());
    println!("{}", part_two(&input).unwrap());
}

pub fn part_one(input: &str) -> Option<Int> {
    Some(
        Records::parse(input)
            .records
            .iter()
            .map(|record| record.num_times())
            .product(),
    )
}

pub fn part_two(input: &str) -> Option<Int> {
    Some(
        Records::parse_part_2(input)
            .records
            .iter()
            .map(|record| record.num_times())
            .product(),
    )
}

#[derive(Debug)]
struct Records {
    records: Vec<Record>,
}

fn parse_line(line: &str) -> Vec<Int> {
    line.split_once(':')
        .unwrap()
        .1
        .split_whitespace()
        .flat_map(|time| time.parse::<Int>())
        .collect_vec()
}

fn parse_line_part_2(line: &str) -> Vec<Int> {
    vec![line
        .split_once(':')
        .unwrap()
        .1
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect::<String>()
        .parse::<Int>()
        .unwrap()]
}

impl Records {
    fn parse(input: &str) -> Self {
        let v = input.lines().map(parse_line).collect_vec();
        Self {
            records: v[0]
                .iter()
                .zip(&v[1])
                .map(|(&time, &distance)| Record { time, distance })
                .collect_vec(),
        }
    }
    fn parse_part_2(input: &str) -> Self {
        let v = input.lines().map(parse_line_part_2).collect_vec();
        Self {
            records: v[0]
                .iter()
                .zip(&v[1])
                .map(|(&time, &distance)| Record { time, distance })
                .collect_vec(),
        }
    }
}

#[derive(Debug)]
struct Record {
    time: Int,
    distance: Int,
}

impl Record {
    // `(time - t) * t > d`
    // Solve: `t**2 - t*time + (d + epsilon) = 0`
    fn min_max_times(&self) -> (Int, Int) {
        let eps = 0.001;
        let det: Float =
            Float::sqrt(((self.time * self.time) as Float) - 4. * (self.distance as Float + eps));
        let lower: Float = (self.time as Float - det) / 2.;
        let upper: Float = (self.time as Float + det) / 2.;
        (lower.ceil() as Int, upper.floor() as Int)
    }
    fn num_times(&self) -> Int {
        let (min, max) = self.min_max_times();
        max - min + 1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_record() {
        let record = Record {
            time: 7,
            distance: 9,
        };
        assert_eq!(record.min_max_times(), (2, 5));
        assert_eq!(record.num_times(), 4);
        let record = Record {
            time: 15,
            distance: 40,
        };
        assert_eq!(record.num_times(), 8);
        let record = Record {
            time: 30,
            distance: 200,
        };
        assert_eq!(record.num_times(), 9);
    }

    #[test]
    fn test_part_one() {
        let result =
            part_one(&std::fs::read_to_string(format!("data/examples/{0:02}.txt", 6)).unwrap());
        assert_eq!(result, Some(288));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&std::fs::read_to_string(format!("data/examples/{0:02}.txt", 6)).unwrap());
        assert_eq!(result, Some(71503));
    }
}
