# --- Load or define supporting classes before this test ---
# RSString, RSNumber, RSMutableDictionary, RSKeyValuePair などが必要

# --- テスト開始 ---

# テスト用キーとバリューを作成
key1 <- RSString$stringWith("id")
key2 <- RSString$stringWith("name")
val1 <- RSNumber$numberWith(123)
val2 <- RSString$stringWith("Taro")


# --- 1. 正常系テスト：objects_and_keys経由で初期化 ---
dict1 <- RSDictionary$new(
  objects_and_keys = list(key1, val1, key2, val2)
)

# 期待する: count() が 2
stopifnot(dict1$count() == 2)

# allKeys() が [key1, key2]
keys <- dict1$allKeys()
stopifnot(length(keys) == 2)
stopifnot(keys[[1]]$isEqual(key1) || keys[[2]]$isEqual(key1))
stopifnot(keys[[1]]$isEqual(key2) || keys[[2]]$isEqual(key2))

# allValues() が [val1, val2]
values <- dict1$allValues()
stopifnot(length(values) == 2)
stopifnot(values[[1]]$isEqual(val1) || values[[2]]$isEqual(val1))
stopifnot(values[[1]]$isEqual(val2) || values[[2]]$isEqual(val2))

# objectForKey が正しく取れる
v1_get <- dict1$objectForKey(key1)
stopifnot(!is.null(v1_get))
stopifnot(v1_get$isEqual(val1))

v2_get <- dict1$objectForKey(key2)
stopifnot(!is.null(v2_get))
stopifnot(v2_get$isEqual(val2))

# --- 2. エラー系テスト：immutableなので変更操作はエラーになる ---
error_caught <- FALSE
tryCatch({
  dict1$setObjectForKey(RSString$scalarWith("X"), key1)
}, error = function(e) {
  error_caught <<- TRUE
})
stopifnot(error_caught)

error_caught <- FALSE
tryCatch({
  dict1$removeObjectForKey(key1)
}, error = function(e) {
  error_caught <<- TRUE
})
stopifnot(error_caught)

error_caught <- FALSE
tryCatch({
  dict1$removeAllObjects()
}, error = function(e) {
  error_caught <<- TRUE
})
stopifnot(error_caught)

# --- 3. 空の辞書を初期化 ---
empty_dict <- RSDictionary$new()

stopifnot(empty_dict$count() == 0)
stopifnot(length(empty_dict$allKeys()) == 0)
stopifnot(length(empty_dict$allValues()) == 0)

# --- テスト完了 ---
cat("All RSDictionary tests passed!\n")
