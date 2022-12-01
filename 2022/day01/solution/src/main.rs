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
    input
        .split("\n\n")
        .map(|block| block.lines().map(|i| i.parse()).collect())
        .collect()
}
