# test_RSDynamicHash
cat("=== RSDynamicHash Test Start ===\n")

# ライブラリやクラス定義が読み込まれている前提

# ハッシュテーブル作成
hash_table <- RSDynamicHash$new(initial_bits = 2)  # 小さめにしてすぐリハッシュ起きるように

# テストオブジェクト作成
num1 <- RSNumber$numberWith(42)
num2 <- RSNumber$numberWith(43)
str1 <- RSString$stringWith("hello")
str2 <- RSString$stringWith("world")

# set動作テスト
hash_table$setObject(num1)
hash_table$setObject(str1)
stopifnot(hash_table$count() == 2)

# get動作テスト
found_num1 <- hash_table$getObject(RSNumber$numberWith(42))
stopifnot(found_num1$isEqual(num1))

found_str1 <- hash_table$getObject(RSString$stringWith("hello"))
stopifnot(found_str1$isEqual(str1))

# 存在しないキーでget
not_found <- hash_table$getObject(RSNumber$numberWith(999))
stopifnot(is.null(not_found))

# remove動作テスト
hash_table$removeObject(num1)
stopifnot(is.null(hash_table$getObject(num1)))
stopifnot(hash_table$count() == 1)

# setで上書きされるかテスト（同じキー）
str1_new <- RSString$stringWith("hello")  # 同じ値
hash_table$setObject(str1_new)  # 上書き
stopifnot(hash_table$count() == 1)  # カウントは変わらない

# リハッシュテスト
# （初期バケットが4個だから、3個以上登録するとリハッシュが起きるはず）
hash_table$setObject(RSString$stringWith("foo"))
hash_table$setObject(RSString$stringWith("bar"))
# この時点でリハッシュが発動して、バケットサイズが増えているはず
stopifnot(hash_table$size >= 8)  # バケットサイズ2倍

cat("=== RSDynamicHash Test Complete ===\n")
