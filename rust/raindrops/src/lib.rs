pub fn raindrops(n: u32) -> String {
    let result = [
        (3, "Pling".to_owned()),
        (5, "Plang".to_owned()),
        (7, "Plong".to_owned()),
    ]
    .iter()
    .filter(|(k, _)| n % *k == 0)
    .map(|(_, v)| v)
    .fold(String::new(), |mut acc, v| {
        acc.push_str(v);
        acc
    });

    if result.is_empty() {
        n.to_string()
    } else {
        result.to_owned()
    }
}
