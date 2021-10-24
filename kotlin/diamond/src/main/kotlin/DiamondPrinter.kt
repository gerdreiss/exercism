class DiamondPrinter {
    fun printToList(char: Char): List<String>? {
        var chars = ('A'..char)
        var idxs = 0..chars.count()
        chars.foldIndexed(mutableListOf<String>()) { (idx, acc, c) ->
            acc
        }
        return listOf()
    }
}
