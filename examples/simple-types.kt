
typealias StringAlias = string
typealias TransitiveStringAlias = StringAlias
typealias ArrayOnAlias = Array<StringAlias>

inline data class StringAlias_alt(val value: String)
inline data class TransitiveString_alt(val value: StringAlias_alt)
inline data class ArrayOnAlias_alt(val value: Array<StringAlias_alt>)

data class SimpleObject(prop: string)
enum class SimpleEnum { A, B, C }
const ConstantString = "CONSTANT_VALUE"

