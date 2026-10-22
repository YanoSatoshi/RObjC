test_that("RSString: value() returns original string", {
  str <- RSString$new("hello")
  expect_equal(str$value(), "hello")
})

test_that("RSString: compare() respects lexicographic order", {
  a <- RSString$new("apple")
  b <- RSString$new("banana")
  c <- RSString$new("banana")
  expect_equal(a$compare(b), -1)
  expect_equal(b$compare(a), 1)
  expect_equal(b$compare(c), 0)
})

test_that("RSString stores factor inputs as character labels", {
  factor_value <- factor("z", levels = c("a", "z"))
  text <- RSString_withValue("z")
  earlier <- RSString_withValue("a")

  for (string in list(RSString$new(factor_value), RSString_withValue(factor_value))) {
    expect_identical(string$value(), "z")
    expect_identical(string$nativeValue(), "z")
    expect_true(string$isEqual(text))
    expect_identical(string$hash(), text$hash())
    expect_identical(string$compare(earlier), 1L)
  }
  factor_object <- RSFactor_withFactor(factor_value)
  expect_identical(factor_object$nativeValue(), factor_value)
})

test_that("RSString: isEqual() matches identical string", {
  a <- RSString$new("test")
  b <- RSString$new("test")
  c <- RSString$new("diff")
  expect_true(a$isEqual(b))
  expect_false(a$isEqual(c))
})

test_that("RSString: isEqual() rejects incompatible objects without an error", {
  string <- RSString_withValue("value")
  expect_false(string$isEqual(RSNumber_withValue(1)))
  expect_false(string$isEqual(RSNull$null))
  expect_false(string$isEqual(NULL))
  expect_error(string$compare(RSNull$null), "Scalar classes are not comparable")
})

test_that("RSString: hash() returns consistent integer", {
  a <- RSString$new("hashme")
  b <- RSString$new("hashme")
  expect_type(a$hash(), "integer")
  expect_equal(a$hash(), b$hash())
})

test_that("RSScalar_withValue('char') returns RSString", {
  wrapped <- RSScalar_withValue("X")
  expect_true(wrapped$isKindOf(RSString))
  expect_equal(wrapped$value(), "X")
})

test_that("RSNAString: value() returns NA_character_", {
  na_str <- RSString$na
  expect_true(is.na(na_str$value()))
})

test_that("RSNAString: compare() with na.last = TRUE/FALSE", {
  s <- RSString$new("abc")
  na <- RSString$na
  expect_equal(na$compare(s, na.last = TRUE), 1)
  expect_equal(na$compare(s, na.last = FALSE), -1)
  expect_equal(s$compare(na, na.last = TRUE), -1)
  expect_equal(s$compare(na, na.last = FALSE), 1)
  expect_equal(na$compare(na, na.last = TRUE), 0)
})

test_that("RSNAString: isEqual() true for same NA class", {
  a <- RSString$na
  b <- RSString$na
  expect_true(a$isEqual(b))
})


test_that("RSNAString: hash() consistent for all NA instances", {
  a <- RSString$na
  b <- RSString$na
  expect_equal(a$hash(), b$hash())
})
