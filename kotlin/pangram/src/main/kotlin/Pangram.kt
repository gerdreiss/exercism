object Pangram {

    fun isPangram(s: String): Boolean =
            "abcdefghijklmnopqrstuvwxyz".toSortedSet() == s.toLowerCase().filter { it.isLetter() }.toSortedSet()

    fun isPangram_better(s: String): Boolean =
        ('a'..'z').all { s.contains(it, ignoreCase = true) }

}