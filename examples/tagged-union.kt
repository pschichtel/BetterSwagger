
sealed class Union(val type: String) {
    data class A(values: Array<String>) : Union("A")
    data class B(prop: String) : Union("B")
}