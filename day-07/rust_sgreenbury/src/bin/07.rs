use anyhow::anyhow;
use hashbrown::HashMap;
use itertools::Itertools;
use std::{cmp::Ordering, str::FromStr};

#[cfg(not(feature = "minimal"))]
advent_of_code::solution!(7);

#[cfg(feature = "minimal")]
fn main() {
    let input = std::fs::read_to_string(format!("data/inputs/{0:02}.txt", 7)).unwrap();
    println!("{}", part_one(&input).unwrap());
    println!("{}", part_two(&input).unwrap());
}

pub fn part_one(input: &str) -> Option<u32> {
    Some(
        input
            .lines()
            .flat_map(|line| line.parse::<Hand<CardPart1>>())
            .sorted()
            .enumerate()
            .map(|(idx, hand)| u32::try_from(idx + 1).unwrap() * hand.bid)
            .sum(),
    )
}

pub fn part_two(input: &str) -> Option<u32> {
    Some(
        input
            .lines()
            .flat_map(|line| line.parse::<Hand<CardPart2>>())
            .sorted()
            .enumerate()
            .map(|(idx, hand)| u32::try_from(idx + 1).unwrap() * hand.bid)
            .sum(),
    )
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Hash, Clone, Copy)]
enum CardPart1 {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Hash, Clone, Copy)]
enum CardPart2 {
    Joker,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Queen,
    King,
    Ace,
}

impl TryFrom<char> for CardPart1 {
    type Error = anyhow::Error;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '2' => Ok(Self::Two),
            '3' => Ok(Self::Three),
            '4' => Ok(Self::Four),
            '5' => Ok(Self::Five),
            '6' => Ok(Self::Six),
            '7' => Ok(Self::Seven),
            '8' => Ok(Self::Eight),
            '9' => Ok(Self::Nine),
            'T' => Ok(Self::Ten),
            'J' => Ok(Self::Jack),
            'Q' => Ok(Self::Queen),
            'K' => Ok(Self::King),
            'A' => Ok(Self::Ace),
            _ => Err(anyhow!("Invalid card char: {value}")),
        }
    }
}

impl TryFrom<char> for CardPart2 {
    type Error = anyhow::Error;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'J' => Ok(Self::Joker),
            '2' => Ok(Self::Two),
            '3' => Ok(Self::Three),
            '4' => Ok(Self::Four),
            '5' => Ok(Self::Five),
            '6' => Ok(Self::Six),
            '7' => Ok(Self::Seven),
            '8' => Ok(Self::Eight),
            '9' => Ok(Self::Nine),
            'T' => Ok(Self::Ten),
            'Q' => Ok(Self::Queen),
            'K' => Ok(Self::King),
            'A' => Ok(Self::Ace),
            _ => Err(anyhow!("Invalid card char: {value}")),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    Three,
    FullHouse,
    Four,
    Five,
}
#[derive(Debug, PartialEq, Eq)]
struct Hand<T: PartialOrd + Ord> {
    cards: Vec<T>,
    bid: u32,
    hand_type: HandType,
}

impl<T: Ord> PartialOrd for Hand<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Ord> Ord for Hand<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.hand_type.cmp(&other.hand_type) {
            Ordering::Greater => Ordering::Greater,
            Ordering::Less => Ordering::Less,
            Ordering::Equal => self
                .cards
                .iter()
                .zip(other.cards.iter())
                .map(|(s, o)| s.cmp(o))
                .take_while_inclusive(|order| matches!(order, Ordering::Equal))
                .last()
                .unwrap(),
        }
    }
}

impl FromStr for Hand<CardPart1> {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut it = s.split_whitespace();
        let cards = it
            .next()
            .unwrap()
            .chars()
            .map(|c| c.try_into())
            .collect::<Result<Vec<CardPart1>, _>>()?;
        if cards.len() != 5 {
            return Err(anyhow!("Invalid number of cards: {}", cards.len()));
        }
        let bid = it.next().ok_or(anyhow!("Missing bid."))?.parse::<u32>()?;
        let counts = cards.iter().fold(HashMap::new(), |mut acc, &card| {
            acc.entry(card).and_modify(|v| *v += 1).or_insert(1);
            acc
        });
        let mut values = counts.values().sorted();
        let hand_type = match (values.nth_back(0), values.nth_back(0)) {
            (Some(5), _) => HandType::Five,
            (Some(4), _) => HandType::Four,
            (Some(&x), Some(&y)) if x == 3 && y == 2 => HandType::FullHouse,
            (Some(&x), Some(&y)) if x == 3 && y == 1 => HandType::Three,
            (Some(&x), Some(&y)) if x == 2 && y == 2 => HandType::TwoPair,
            (Some(&x), Some(&y)) if x == 2 && y == 1 => HandType::OnePair,
            (Some(1), _) => HandType::HighCard,
            _ => return Err(anyhow!("Invalid card counts.")),
        };
        Ok(Self {
            cards,
            bid,
            hand_type,
        })
    }
}

impl FromStr for Hand<CardPart2> {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut it = s.split_whitespace();
        let cards = it
            .next()
            .unwrap()
            .chars()
            .map(|c| c.try_into())
            .collect::<Result<Vec<CardPart2>, _>>()?;
        if cards.len() != 5 {
            return Err(anyhow!("Invalid number of cards: {}", cards.len()));
        }
        let bid = it.next().ok_or(anyhow!("Missing bid."))?.parse::<u32>()?;
        let mut counts = cards.iter().fold(HashMap::new(), |mut acc, &card| {
            acc.entry(card).and_modify(|v| *v += 1).or_insert(1);
            acc
        });
        if let Some(jokers) = counts.get(&CardPart2::Joker) {
            (0..*jokers).for_each(|_| {
                if let Some(max_key) = counts
                    .iter()
                    .filter(|(&k, &v)| (v < 5) && (k != CardPart2::Joker))
                    .max_by(|(_, v1), (_, v2)| v1.cmp(v2))
                    .map(|(&k, _)| k)
                {
                    counts.entry(max_key).and_modify(|v| *v += 1);
                    counts.entry(CardPart2::Joker).and_modify(|v| *v -= 1);
                }
            });
        }
        let mut values = counts.values().sorted();
        let hand_type = match (values.nth_back(0), values.nth_back(0)) {
            (Some(5), _) => HandType::Five,
            (Some(4), _) => HandType::Four,
            (Some(&x), Some(&y)) if x == 3 && y == 2 => HandType::FullHouse,
            (Some(&x), Some(&y)) if x == 3 && y == 1 => HandType::Three,
            (Some(&x), Some(&y)) if x == 2 && y == 2 => HandType::TwoPair,
            (Some(&x), Some(&y)) if x == 2 && y == 1 => HandType::OnePair,
            (Some(1), _) => HandType::HighCard,
            _ => return Err(anyhow!("Invalid card counts.")),
        };
        Ok(Self {
            cards,
            bid,
            hand_type,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ord() {
        let hand1 = "33332 2";
        let hand2 = "22AAA 0";
        assert!(
            hand1.parse::<Hand<CardPart1>>().unwrap() > hand2.parse::<Hand<CardPart1>>().unwrap()
        )
    }

    #[test]
    fn test_ord2() {
        let hand1 = "JKKK2 0";
        let hand2 = "QQQQ2 0";
        assert!(
            hand1.parse::<Hand<CardPart2>>().unwrap() < hand2.parse::<Hand<CardPart2>>().unwrap()
        )
    }

    #[test]
    fn test_hand2() {
        let hand1 = "J9555 0";
        assert_eq!(
            hand1.parse::<Hand<CardPart2>>().unwrap().hand_type,
            HandType::Four
        );
    }

    #[test]
    fn test_part_one() {
        let result =
            part_one(&std::fs::read_to_string(format!("data/examples/{0:02}.txt", 7)).unwrap());
        assert_eq!(result, Some(6440));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&std::fs::read_to_string(format!("data/examples/{0:02}.txt", 7)).unwrap());
        assert_eq!(result, Some(5905));
    }
}
