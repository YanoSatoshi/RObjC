# テスト用データ
str1 <- RSString$stringWith("str1")
str2 <- RSString$stringWith("str2")
str3 <- RSString$stringWith("str3")

# 初期化
mset <- RSMutableSet$new(list(str1, str2))
stopifnot(mset$count() == 2)

# addObject
mset$addObject(str3)
stopifnot(mset$count() == 3)
stopifnot(mset$containsObject(str3))

# removeObject
mset$removeObject(str1)
stopifnot(!mset$containsObject(str1))
stopifnot(mset$count() == 2)

# clear
mset$clear()
stopifnot(mset$count() == 0)
stopifnot(length(mset$allObjects()) == 0)

# 再追加 & freeze 検証
mset$addObject(str2)
frozen <- RSSet$new(mset$allObjects())  # Objective-Cスタイル
stopifnot(frozen$isKindOf(RSSet))
stopifnot(frozen$containsObject(str2))
