cat("Start testing RSMutableDictionary initialize()...\n")

# 普通のRSKeyValuePair listで初期化
key1 <- RSString$stringWith("id")
val1 <- RSNumber$numberWith(1)
key2 <- RSString$stringWith("name")
val2 <- RSString$stringWith("Taro")

entry1 <- RSKeyValuePair$new(key1, val1)
entry2 <- RSKeyValuePair$new(key2, val2)

dict1 <- RSMutableDictionary$new(key_value_pairs = list(entry1, entry2))

stopifnot(dict1$count() == 2)
stopifnot(dict1$objectForKey(key1)$isEqual(val1))
stopifnot(dict1$objectForKey(key2)$isEqual(val2))

# atomicキー・値交互リストで初期化
dict2 <- RSMutableDictionary$new(
  objects_and_keys = vapply(
    list("id", 1, "name", "Taro"),
    function(v) list(RSScalar$valueWith(v)), list(1)
  )
)

stopifnot(dict2$count() == 2)
stopifnot(dict2$objectForKey(RSString$stringWith("id"))$value() == 1)
stopifnot(dict2$objectForKey(RSString$stringWith("name"))$value() == "Taro")

# keys/values分離で初期化
dict3 <- RSMutableDictionary$new(
  keys = vapply(list("a", "b", "c"), function(v) list(RSScalar$valueWith(v)), list(1)),
  values = vapply(list(1, 2, 3), function(v) list(RSScalar$valueWith(v)), list(1))
)

stopifnot(dict3$count() == 3)
stopifnot(dict3$objectForKey(RSScalar$valueWith("b"))$value() == 2)

cat("All RSMutableDictionary initialize() tests passed!\n")




# テスト開始
cat("Start testing RSMutableDictionary...\n")

# 新規作成
dict <- RSMutableDictionary$new()

# 追加
key1 <- RSString$stringWith("key1")
val1 <- RSNumber$numberWith(10)
key2 <- RSString$stringWith("key2")
val2 <- RSString$stringWith("hello")

dict$setObjectForKey(val1, key1)
dict$setObjectForKey(val2, key2)

# 値取得
stopifnot(dict$objectForKey(key1)$isEqual(val1))
stopifnot(dict$objectForKey(key2)$isEqual(val2))

# キー・値リスト
keys <- dict$allKeys()
values <- dict$allValues()
stopifnot(length(keys) == 2)
stopifnot(length(values) == 2)

# カウント
stopifnot(dict$count() == 2)

# 削除
dict$removeObjectForKey(key1)
stopifnot(is.null(dict$objectForKey(key1)))
stopifnot(dict$count() == 1)

# 全削除
dict$removeAllObjects()
stopifnot(dict$count() == 0)

# マージ
dict1 <- RSMutableDictionary$new()
dict2 <- RSMutableDictionary$new()

k1 <- RSString$stringWith("a")
v1 <- RSString$stringWith("A")
k2 <- RSString$stringWith("b")
v2 <- RSString$stringWith("B")
k3 <- RSString$stringWith("c")
v3 <- RSString$stringWith("C")

dict1$setObjectForKey(v1, k1)
dict1$setObjectForKey(v2, k2)
dict2$setObjectForKey(v2, k2) # かぶり
dict2$setObjectForKey(v3, k3)

merged <- dict1$dictionaryByMergingWithDictionary(dict2)
stopifnot(merged$count() == 3)

# 除外
excluded <- dict1$dictionaryByExcludingWithDictionary(dict2)
stopifnot(excluded$count() == 1)

cat("All RSMutableDictionary tests passed!\n")
