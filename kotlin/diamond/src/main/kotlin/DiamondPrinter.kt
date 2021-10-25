class DiamondPrinter {
    fun printToList(char: Char): List<String>? {
        val chars = ('A'..char)
        val lines = chars.withIndex().map { printChar(it.value, it.index, chars.count()) }.toList()
        return lines + lines.reversed().drop(1)
    }

    private fun printChar(char: Char, index: Int, chars: Int): String =
        if (index == 0) spaces(chars - 1) + "A" + spaces(chars - 1)
        else spaces(chars - index - 1) +
                char.toString() +
                spaces(index + index - 1) +
                char.toString() +
                spaces(chars - index - 1)

    private fun spaces(n: Int): String = " ".repeat(n)
}