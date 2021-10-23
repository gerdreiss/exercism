use std::collections::HashMap;

pub fn can_construct_note(magazine: &[&str], note: &[&str]) -> bool {
    let from_m = word_count(magazine);
    let from_n = word_count(note);

    // mine
    // from_n.iter().all(|(word, count)| {
    //     if let Some(c) = from_m.get(word) {
    //         c >= count
    //     } else {
    //         false
    //     }
    // })
    // better
    from_n
        .iter()
        .all(&|(w, count)| from_m.get(w).unwrap_or(&0) >= count)
}

fn word_count<'a>(words: &'a [&str]) -> HashMap<&'a str, usize> {
    let mut result: HashMap<&str, usize> = HashMap::new();
    for w in words {
        if let Some(count) = result.get(w) {
            result.insert(w, count + 1);
        } else {
            result.insert(w, 1);
        }
    }
    result
}

fn word_count_better<'a>(words: &'a [&str]) -> HashMap<&'a str, usize> {
    words
        .iter()
        .fold(HashMap::new(), |mut words, str| {
            *words.entry(str).or_insert(0) += 1;
            words
        })
}
