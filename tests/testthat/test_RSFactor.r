test_that("RSFactor: wraps factor correctly", {
  f <- factor("low", levels = c("low", "medium", "high"), ordered = TRUE)
  rsf <- RSFactor$new(f)
  expect_equal(rsf$value(), "low")
  expect_equal(rsf$levels(), levels(f))
  expect_true(rsf$ordered())
})

test_that("RSFactor: direct construction requires one factor value", {
  expect_error(RSFactor$new(factor(character())),
               "`val` must be a factor scalar.", fixed = TRUE)
  expect_error(RSFactor$new(factor(c("a", "b"))),
               "`val` must be a factor scalar.", fixed = TRUE)
  expect_error(RSFactor$new("a"),
               "`val` must be a factor scalar.", fixed = TRUE)
})

test_that("RSFactor: compare() respects factor ordering", {
  a <- RSFactor$new(factor("low", levels = c("low", "medium", "high"), ordered = TRUE))
  b <- RSFactor$new(factor("medium", levels = c("low", "medium", "high"), ordered = TRUE))
  c <- RSFactor$new(factor("medium", levels = c("low", "medium", "high"), ordered = TRUE))
  expect_equal(a$compare(b), -1)
  expect_equal(b$compare(a), 1)
  expect_equal(b$compare(c), 0)
})

test_that("RSFactor: isEqual() true for same level/value", {
  a <- RSFactor$new(factor("high", levels = c("low", "medium", "high")))
  b <- RSFactor$new(factor("high", levels = c("low", "medium", "high")))
  c <- RSFactor$new(factor("medium", levels = c("low", "medium", "high")))
  expect_true(a$isEqual(b))
  expect_false(a$isEqual(c))
})

test_that("RSFactor key equality uses labels without inventing an order", {
  string <- RSString_withValue("a")
  factor_a <- RSFactor_withFactor(factor("a", levels = c("a", "b")))
  reordered_a <- RSFactor_withFactor(factor("a", levels = c("b", "a")))
  factor_x <- RSFactor_withFactor(factor("x", levels = c("x", "y")))

  expect_true(string$isEqual(factor_a))
  expect_true(factor_a$isEqual(string))
  expect_true(factor_a$isEqual(reordered_a))
  expect_error(factor_a$compare(string), "no shared ordering", fixed = TRUE)
  expect_error(string$compare(factor_a), "no shared ordering", fixed = TRUE)
  expect_error(factor_a$compare(reordered_a),
               "Factor level sets differ", fixed = TRUE)
  expect_false(factor_a$isEqual(factor_x))
  expect_identical(factor_a$hash(), string$hash())
  expect_identical(factor_a$hash(), reordered_a$hash())
  expect_error(factor_a$compare(NULL), "Scalar classes are not comparable", fixed = TRUE)
})

test_that("RSFactor retains factor order for matching levels", {
  levels <- c("z", "a")
  z <- RSFactor_withFactor(factor("z", levels = levels, ordered = TRUE))
  a <- RSFactor_withFactor(factor("a", levels = levels, ordered = TRUE))
  expect_identical(z$compare(a), -1L)
  expect_identical(a$compare(z), 1L)
  expect_identical(z$compare(a, na.last = 1L), -1L)
  expect_identical(z$compare(a, na.last = 0L), -1L)
  expect_identical(a$compare(a), 0L)
  expect_error(z$compare(a, na.last = NA), "`na.last` must be TRUE or FALSE.",
               fixed = TRUE)
  expect_error(z$compare(a, na.last = c(TRUE, FALSE)),
               "`na.last` must be TRUE or FALSE.", fixed = TRUE)
  expect_error(z$compare(a, na.last = "unknown"),
               "`na.last` must be TRUE or FALSE.", fixed = TRUE)

  string <- RSString_withValue("a")
  expect_error(z$compare(string, na.last = 1L),
               "no shared ordering", fixed = TRUE)
})

test_that("RSFactor sorts unordered factors by levels like base R", {
  levels <- c("z", "a")
  z <- RSFactor_withFactor(factor("z", levels = levels))
  a <- RSFactor_withFactor(factor("a", levels = levels))
  expect_identical(z$compare(a), -1L)
  expect_identical(a$compare(z), 1L)
})

test_that("RSFactor: hash() returns consistent int", {
  a <- RSFactor$new(factor("medium", levels = c("low", "medium", "high")))
  b <- RSFactor$new(factor("medium", levels = c("low", "medium", "high")))
  expect_equal(a$hash(), b$hash())
})

test_that("RSScalar_withValue(factor) returns RSFactor", {
  f <- factor("foo", levels = c("foo", "bar"))
  wrapped <- RSScalar_withValue(f)
  expect_true(wrapped$isKindOf(RSFactor))
})

test_that("RSNAFactor: value is NA with factor() wrapper", {
  na <- RSFactor$na
  expect_true(is.na(na$value()))
  expect_identical(na$levels(), character())
  expect_false(na$ordered())
})

test_that("RSFactor_withFactor preserves metadata for missing factors", {
  original <- factor(NA_character_, levels = c("low", "high"), ordered = TRUE)
  a <- RSFactor_withFactor(original)
  b <- RSFactor_withFactor(original)
  expect_false(robjc:::class_isSingleton(RSNAFactor))
  expect_true(a$isMemberOf(RSNAFactor))
  expect_false(identical(a, b))
  expect_identical(a$nativeValue(), original)
  expect_identical(a$levels(), levels(original))
  expect_true(a$ordered())
})

test_that("RSNAFactor: compare() with na.last = TRUE/FALSE", {
  f <- factor("bar", levels = c("foo", "bar", "baz"))
  val <- RSFactor$new(f)
  na <- RSFactor_withFactor(factor(NA_character_, levels = levels(f)))
  expect_equal(na$compare(val, na.last = TRUE), 1)
  expect_equal(na$compare(val, na.last = FALSE), -1)
  expect_equal(val$compare(na, na.last = TRUE), -1)
  expect_equal(val$compare(na, na.last = FALSE), 1)
  expect_equal(na$compare(na, na.last = TRUE), 0)
})

test_that("RSNAFactor: compare rejects different levels even with NA", {
  left <- RSFactor_withFactor(factor(NA_character_, levels = c("a", "b")))
  right <- RSFactor_withFactor(factor(NA_character_, levels = c("x", "y")))
  present <- RSFactor_withFactor(factor("a", levels = c("a", "b")))

  expect_error(left$compare(right), "Factor level sets differ.", fixed = TRUE)
  expect_error(present$compare(right), "Factor level sets differ.", fixed = TRUE)
  expect_error(right$compare(present), "Factor level sets differ.", fixed = TRUE)
})

test_that("RSNAFactor: isEqual() true for same NA class", {
  a <- RSFactor$na
  b <- RSFactor$na
  expect_true(a$isEqual(b))
})

test_that("RSNAFactor: hash() is stable", {
  a <- RSFactor$na
  b <- RSFactor$na
  expect_equal(a$hash(), b$hash())
})
