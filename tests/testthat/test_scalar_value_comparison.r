test_that("nativeCompare returns three-way results after base R coercion", {
  boolean <- RSBoolean_withBoolean(TRUE)
  number <- RSNumber_withValue(2)
  string <- RSString_withValue("10")

  expect_identical(boolean$nativeCompare(number), -1L)
  expect_identical(number$nativeCompare(string), 1L)
  expect_identical(number$nativeCompare(RSNumber_withValue(2)), 0L)
  expect_identical(RSRaw_withValue(as.raw(1))$nativeCompare(
    RSRaw_withValue(as.raw(2))
  ), -1L)
  expect_error(number$nativeCompare(2), "RSScalar object")
  expect_error(number$nativeCompare(string, NA), "`na.last`")
})

test_that("nativeCompare preserves factor and complex base R behavior", {
  factor <- RSFactor_withFactor(factor("a", levels = c("a", "b")))
  string <- RSString_withValue("a")
  expect_identical(factor$value(), "a")
  expect_identical(factor$nativeValue(), factor$asFactor())
  expect_true(is.factor(factor$nativeValue()))
  expect_identical(string$nativeValue(), "a")
  expect_identical(factor$nativeCompare(string), 0L)
  expect_warning(
    expect_error(factor$nativeCompare(RSString_withValue("b")),
                 "no ordering"),
    "not meaningful for factors"
  )

  ordered <- RSFactor_withFactor(ordered("z", levels = c("z", "a")))
  expect_identical(ordered$nativeCompare(string), -1L)

  complex <- RSComplex_withValue(1 + 2i)
  expect_identical(complex$nativeValue(), 1 + 2i)
  expect_identical(complex$value(), 1 + 2i)
  expect_identical(complex$nativeCompare(RSComplex_withValue(1 + 2i)), 0L)
  expect_error(complex$nativeCompare(RSComplex_withValue(1 + 3i)),
               "invalid comparison with complex values")
})

test_that("nativeCompare follows compare's missing and NULL policy", {
  missing <- RSNumber$na
  nan <- RSNumber_withValue(NaN)
  regular <- RSNumber_withValue(1)
  expect_identical(missing$nativeCompare(missing), 0L)
  expect_identical(nan$nativeCompare(nan), 0L)
  expect_identical(missing$nativeCompare(nan), 0L)
  expect_identical(missing$nativeCompare(regular), 1L)
  expect_identical(missing$nativeCompare(regular, na.last = FALSE), -1L)
  expect_identical(regular$nativeCompare(missing), -1L)
  expect_identical(regular$nativeCompare(missing, na.last = FALSE), 1L)
  expect_identical(RSNull$null$nativeCompare(RSNull$null), 0L)
  expect_error(RSNull$null$nativeCompare(regular), "not comparable")
  expect_error(regular$nativeCompare(RSNull$null), "not comparable")
})

test_that("key equality remains independent of three-way ordering", {
  expect_false(RSBoolean_withBoolean(TRUE)$isEqual(RSNumber_withValue(1)))
  expect_true(RSFactor_withFactor(factor("a"))$isEqual(RSString_withValue("a")))
  expect_true(RSComplex_withValue(1 + 2i)$isEqual(RSComplex_withValue(1 + 2i)))
  expect_false(RSComplex_withValue(1 + 2i)$isEqual(RSComplex_withValue(1 + 3i)))
  expect_true(RSComplex$na$isEqual(RSComplex$na))
  expect_identical(
    RSComplex_withValue(1 + 2i)$compare(RSComplex_withValue(1 + 3i)), -1L
  )
})

test_that("NaN remains a value and has object key equality distinct from NA", {
  first <- RSNumber_withValue(NaN)
  same <- RSNumber_withValue(NaN)
  expect_true(first$isMemberOf(RSNumber))
  expect_true(is.nan(first$value()))
  expect_true(first$isEqual(same))
  expect_false(first$isEqual(RSNumber$na))
  expect_false(RSNumber$na$isEqual(first))
  expect_false(first$isEqual(RSNumber_withValue(Inf)))
  expect_identical(first$hash(), 0L)
  expect_true(RSNumber$na$isEqual(RSNumber$na))
  expect_true(RSNull$null$isEqual(RSNull$null))
  expect_true(RSNumber_withValue(1L)$isEqual(RSNumber_withValue(1)))

  dictionary <- RSDictionary$new(
    keys = list(first, RSNumber$na),
    values = list(RSString_withValue("nan"), RSString_withValue("na"))
  )
  expect_identical(dictionary$objectForKey(same)$value(), "nan")
  expect_identical(dictionary$objectForKey(RSNumber$na)$value(), "na")
})

test_that("complex NaN keys preserve their other component", {
  first <- RSComplex_withValue(complex(real = NaN, imaginary = 1))
  same <- RSComplex_withValue(complex(real = NaN, imaginary = 1))
  different <- RSComplex_withValue(complex(real = NaN, imaginary = 2))
  mixed <- RSComplex_withValue(complex(real = NaN, imaginary = NA_real_))
  expect_true(first$isMemberOf(RSComplex))
  expect_true(is.nan(first$value()))
  expect_true(first$isEqual(same))
  expect_false(first$isEqual(different))
  expect_false(first$isEqual(RSComplex$na))
  expect_false(RSComplex$na$isEqual(first))
  expect_true(is.nan(mixed$value()))
  expect_false(mixed$isEqual(RSComplex$na))
  expect_identical(first$hash(), 0L)
  expect_identical(mixed$hash(), 0L)
})
