use std::num::ParseIntError;

fn main() -> std::io::Result<()> {
    let path = std::env::args().nth(1).expect("should have file path");
    println!("File: {}", path);

    let input = std::fs::read_to_string(path)?;
    let parsed = parse(input).unwrap();
    let mut sums: Vec<u32> = parsed.iter().map(|p| p.iter().sum()).collect();
    sums.sort();

    println!("{:?}", sums.iter().rev().take(1).sum::<u32>());
    println!("{:?}", sums.iter().rev().take(3).sum::<u32>());
    Ok(())
}

fn parse(input: String) -> Result<Vec<Vec<u32>>, ParseIntError> {
    blocks(&input)
        .map(|block| block.iter().map(|i| i.parse()).collect())
        .collect()
}

fn blocks<'a>(string: &'a str) -> Blocks<'a, '_> {
    Blocks {
        split: string.split("\n\n"),
    }
}

struct Blocks<'a, 'b> {
    split: std::str::Split<'a, &'b str>,
}

impl<'a> Iterator for Blocks<'a, '_> {
    type Item = Vec<&'a str>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.split.next()?.lines().collect())
    }
}
