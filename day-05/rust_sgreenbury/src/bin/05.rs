use itertools::Itertools;
use range_set_blaze::RangeSetBlaze;

#[cfg(not(feature = "minimal"))]
advent_of_code::solution!(5);

type Int = i64;

#[cfg(feature = "minimal")]
fn main() {
    let input = std::fs::read_to_string(format!("data/inputs/{0:02}.txt", 5)).unwrap();
    println!("{}", part_one(&input).unwrap());
    println!("{}", part_two(&input).unwrap());
}

pub fn part_one(input: &str) -> Option<Int> {
    Some(SeedMap::parse(input).min_location())
}

pub fn part_two(input: &str) -> Option<Int> {
    Some(SeedMap::parse(input).min_location_part_2())
}

#[derive(Debug)]
struct SeedMap {
    seeds: Vec<Int>,
    maps: Vec<RangeSet>,
}

// TODO: use FromStr
impl SeedMap {
    fn parse(input: &str) -> Self {
        let mut it = input.split("\n\n");
        let seeds = it
            .next()
            .unwrap()
            .split('\n')
            .next()
            .unwrap()
            .split(": ")
            .nth(1)
            .unwrap()
            .split(' ')
            .flat_map(|el| el.parse::<Int>())
            .collect_vec();
        let mut maps: Vec<RangeSet> = vec![];
        it.for_each(|set_str| {
            let mut set: Vec<RangeMap> = vec![];
            let ranges = set_str.split('\n').skip(1).filter(|line| !line.is_empty());
            ranges.for_each(|line| {
                let (destination, source, range) = line
                    .splitn(3, ' ')
                    .flat_map(|el| el.parse::<Int>())
                    .collect_tuple()
                    .unwrap();

                let mut rsb = RangeSetBlaze::new();
                rsb.ranges_insert(source..=(source + range - 1));
                set.push(RangeMap {
                    source: rsb,
                    offset: destination - source,
                })
            });
            maps.push(RangeSet { set });
        });
        Self { seeds, maps }
    }
}

impl SeedMap {
    fn map(&self, seed: Int) -> Int {
        self.maps.iter().fold(seed, |acc, map| map.map(acc))
    }
    fn min_location(&self) -> Int {
        self.seeds.iter().map(|seed| self.map(*seed)).min().unwrap()
    }
    fn map_part_2(&self, seed: RangeSetBlaze<Int>) -> RangeSetBlaze<Int> {
        self.maps.iter().fold(seed, |acc, map| map.map_range(&acc))
    }
    fn min_location_part_2(&self) -> Int {
        let seed_ranges = self
            .seeds
            .chunks(2)
            .fold(RangeSetBlaze::new(), |mut acc, chunk| {
                let first = *chunk.first().unwrap();
                let last = *chunk.first().unwrap() + *chunk.last().unwrap();
                acc.ranges_insert(first..=(last - 1));
                acc
            });
        self.map_part_2(seed_ranges)
            .ranges()
            .map(|r| *r.start())
            .min()
            .unwrap()
    }
}

#[derive(Debug)]
struct RangeSet {
    set: Vec<RangeMap>,
}
#[derive(Debug)]
struct RangeMap {
    source: RangeSetBlaze<Int>,
    offset: Int,
}

impl RangeSet {
    fn map(&self, seed: Int) -> Int {
        for set in self.set.iter() {
            if set.source.contains(seed) {
                return seed + set.offset;
            }
        }
        seed
    }
    fn map_range(&self, seed_range: &RangeSetBlaze<Int>) -> RangeSetBlaze<Int> {
        seed_range
            .to_owned()
            .into_ranges()
            .fold(RangeSetBlaze::new(), |mut destinations, input| {
                let mut input_range_set = RangeSetBlaze::new();
                input_range_set.ranges_insert(input);
                let sources = self
                    .set
                    .iter()
                    .fold(RangeSetBlaze::<Int>::new(), |acc, set| {
                        acc | set.source.to_owned()
                    });

                // Start with ones that are not mapped
                destinations |= seed_range - sources;

                // Loop over intersections and offset
                for set in self.set.iter() {
                    let intersection = input_range_set.to_owned() & set.source.to_owned();
                    destinations |= intersection
                        .into_ranges()
                        .map(|r| r.start() + set.offset..(r.end() + set.offset))
                        .fold(RangeSetBlaze::new(), |mut acc, r| {
                            acc.ranges_insert(r.start..=(r.end - 1));
                            acc
                        });
                }
                destinations
            })
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result =
            part_one(&std::fs::read_to_string(format!("data/examples/{0:02}.txt", 5)).unwrap());
        assert_eq!(result, Some(35));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&std::fs::read_to_string(format!("data/examples/{0:02}.txt", 5)).unwrap());
        assert_eq!(result, Some(46));
    }
}
