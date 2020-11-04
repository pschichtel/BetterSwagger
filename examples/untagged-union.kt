
sealed class Union {
    data class A(val items: List<String>) : Union()
    data class B(val prop: String) : Union()
    data class StringData(val s: String) : Union()
}
