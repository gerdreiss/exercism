use std::collections::{HashMap, HashSet};

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
