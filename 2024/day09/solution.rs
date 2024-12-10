fn main() {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).expect("should have been a line");
    let mut result = build_disk(input.trim());

    let mut free = 0;
    let mut next_block = result.len() - 1;

    loop {
        while result[next_block].is_none() {
            next_block -= 1;
        }

        while result[free].is_some() {
            free += 1;
        }

        if free > next_block {
            break;
        }

        let temp = result[free].clone();
        result[free] = result[next_block].clone();
        result[next_block] = temp;
        //println!("{:?}, free: {}, next_block: {}", result, free, next_block);
    }

    let mut sum = 0;
    for (i, x) in result.iter().enumerate() {
        if let Some(x) = x {
            sum += i * x;
        }
    }

    println!("{}", sum);
}

fn build_disk(input: &str) -> Vec<Option<usize>> {
    let mut is_disk = true;
    let mut result = Vec::new();
    let mut id = 0;

    for x in input.chars() {
        let x = x.to_digit(10).expect("should be a digit") as usize;
        for _ in 0..x {
            if is_disk {
                result.push(Some(id));
            } else {
                result.push(None);
            }
        }
        if is_disk {
            id += 1;
        }
        is_disk ^= true;
    }

    result
}
