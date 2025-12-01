fn solve_one(x: i64) -> Vec<i64> {
    if x == 0 {
        return vec![1];
    } else {
        let s = x.to_string();
        let len = s.len();
        if len % 2 == 0 {
            return split(&s)
                .into_iter()
                .map(|part| part.parse::<i64>().unwrap_or(0))
                .collect();
        } else {
            return vec![2024 * x];
        }
    }
}

fn split(arr: &str) -> Vec<String> {
    let half = arr.len() / 2;
    vec![arr[..half].to_string(), arr[half..].to_string()]
}

fn iterate(input: Vec<i64>) -> Vec<i64> {
    let mut result = Vec::new();
    for &i in input.iter() {
        result.extend(solve_one(i));
    }
    result
}

fn main() {
    let mut result = vec![5688, 62084, 2, 3248809, 179, 79, 0, 172169];

    for i in 0..75 {
        result = iterate(result);
        println!("Iteration {}: Size = {}", i, result.len());
    }

    println!("Final size of result: {}", result.len());
}
