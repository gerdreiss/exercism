use std::collections::{HashMap, HashSet};

// my solution
pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    possible_anagrams
        .iter()
        .cloned()
        .filter(|anagram| anagrams(word.to_lowercase(), anagram.to_lowercase()))
        .collect()
}

fn anagrams(word: String, anagram: String) -> bool {
    anagram != word && grouped(word) == grouped(anagram)
}

fn grouped(s: String) -> HashMap<char, usize> {
    let mut stats: HashMap<char, usize> = HashMap::new();
    for ch in s.chars() {
        *stats.entry(ch).or_insert(0) += 1;
    }
    stats
}

// a really nice solution from https://exercism.org/tracks/rust/exercises/anagram/solutions/itkovian
// pub fn anagrams_for<'a>(target: &str, inputs: & [&'a str]) -> HashSet<&'a str> {
//     fn sort_string(s: &str) -> Vec<char> {
//         let mut target_chars: Vec<char> = s.to_lowercase().chars().collect();
//         target_chars.sort();
//         return target_chars;
//     }
//     let target_sorted = sort_string(&target);
//     inputs.iter().cloned()
//         .filter(|s| s.to_lowercase() != target.to_lowercase())
//         .filter(|s| sort_string(s) == target_sorted)
//         .collect()
// }
