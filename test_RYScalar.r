#' test_RSScalar
cat("=== RSScalar Test Start ===\n")

# 数値
num <- RSNumber$numberWith(42)
stopifnot(num$value() == 42)
stopifnot(num$compare(RSNumber$numberWith(43)) == -1)
stopifnot(num$isEqual(RSNumber$numberWith(42)))

# NA数値
na_num <- RSNANumber$new()
stopifnot(is.na(na_num$value()))
stopifnot(is.na(na_num$compare(na_num)))

# 文字列
str <- RSString$stringWith("hello")
stopifnot(str$value() == "hello")
stopifnot(str$isEqual(RSString$stringWith("hello")))
stopifnot(str$compare(RSString$stringWith("world")) == -1)

# NA文字列
na_str <- RSNAString$new()
stopifnot(is.na(na_str$value()))
stopifnot(is.na(na_str$compare(na_str)))

# 論理値
bool_true <- RSBoolean$boolWith(TRUE)
bool_false <- RSBoolean$boolWith(FALSE)
stopifnot(bool_true$value() == TRUE)
stopifnot(bool_false$value() == FALSE)
stopifnot(bool_true$compare(bool_false) == 1)

# NA論理値
na_bool <- RSNABoolean$new()
stopifnot(is.na(na_bool$value()))
stopifnot(is.na(na_bool$compare(na_bool)))

# 因子
fct <- RSFactor$factorWith(factor("apple"))
stopifnot(fct$value() == "apple")
stopifnot(fct$levels()[1] == "apple")
stopifnot(!fct$ordered())

# NA因子
na_fct <- RSNAFactor$new()
stopifnot(is.na(na_fct$value()))

# NULL
null_obj <- RSNull$null()
stopifnot(is.null(null_obj$value()))
stopifnot(null_obj$isEqual(RSNull$null()))

# with() 自動判別
wrapped_num <- RSScalar$valueWith(42)
wrapped_str <- RSScalar$valueWith("hello")
wrapped_bool <- RSScalar$valueWith(TRUE)
wrapped_fct <- RSScalar$valueWith(factor("apple"))
wrapped_null <- RSScalar$valueWith(NULL)
wrapped_na_num <- RSScalar$valueWith(NA_real_)

stopifnot(inherits(wrapped_num, "RSNumber"))
stopifnot(inherits(wrapped_str, "RSString"))
stopifnot(inherits(wrapped_bool, "RSBoolean"))
stopifnot(inherits(wrapped_fct, "RSFactor"))
stopifnot(inherits(wrapped_null, "RSNull"))
stopifnot(inherits(wrapped_na_num, "RSNANumber"))

cat("=== RSScalar Test Complete ===\n")
