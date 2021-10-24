class Year(y: Int) {
    val isLeap by lazy {
        y % 400 == 0 || y % 4 == 0 && y % 100 != 0
    }
}
