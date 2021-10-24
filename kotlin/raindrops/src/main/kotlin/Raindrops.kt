class Raindrops {
    companion object {
        @JvmStatic
        fun convert(input: Int): String = mapOf(
                Pair(3, "Pling"),
                Pair(5, "Plang"),
                Pair(7, "Plong")
        ).map { pair ->
            if (input % pair.key == 0) pair.value else ""
        }.fold(input.toString()) { total, next ->
            total + next
        }.partition { ch ->
            ch.isLetter()
        }.toList().first { it.isNotEmpty() }
    }
}
