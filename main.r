library(devtools)


library(robjc)


op <- RSHashOperator$module()
h1 <- op$hashForScalar("hello")           # スカラー値（文字列）のハッシュ
h2 <- op$computeHashCodeWithString("hello")  # 32bit 符号付き版ハッシュ
print(h2)

A <- list(RSNumber$numberWith(1), RSNumber$numberWith(2))
B <- list(RSNumber$numberWith(2), RSNumber$numberWith(1))
print(op$areEqualSets(A, B))  # => TRUE （順序が逆でも同じ集合と判定）



decoder <- RSClassDecoder$module()
print(decoder)