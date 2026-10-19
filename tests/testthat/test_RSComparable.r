test_that("RSComparable marks classes with an ordering method", {
  expect_true(robjc_isProtocol(RSComparable))
  expect_true(RSNumber_withValue(1)$conformsTo(RSComparable))
  expect_true(RSString_withValue("a")$conformsTo(RSComparable))
  expect_true(RSFactor$new(factor("a"))$conformsTo(RSComparable))
  expect_true(RSNumber$na$conformsTo(RSComparable))
  expect_true(RSNull$null$conformsTo(RSComparable))
  expect_true(RSMutableArray$new()$conformsTo(RSComparable))
  expect_true(RSMutableVector$new(integer())$conformsTo(RSComparable))
  expect_true(RSRaw$new(as.raw(1))$conformsTo(RSComparable))
  expect_false(RSObject$new()$conformsTo(RSComparable))
})

test_that("array comparison skips equal non-comparable elements", {
  protocol <- RSProtocolClass("ComparableArrayProbe")
  object <- RSObject$new()
  left <- RSArray$new(list(RSString, protocol, object,
                           RSNumber_withValue(1)))
  equal <- RSArray$new(list(RSString, protocol, object,
                            RSNumber_withValue(1)))
  greater <- RSArray$new(list(RSString, protocol, object,
                              RSNumber_withValue(2)))
  expect_identical(left$compare(equal), 0L)
  expect_identical(left$compare(greater), -1L)
  expect_identical(greater$compare(left), 1L)
  expect_error(left$compare(RSArray$new(list(RSNumber))),
               "Array elements are not comparable", fixed = TRUE)
  expect_error(RSArray$new(list(RSRaw$new(as.raw(1))))$compare(
    RSArray$new(list(RSRaw$new(as.raw(2))))
  ), "Raw values cannot be sorted by base R", fixed = TRUE)
  raw_value <- RSRaw$new(as.raw(1))
  expect_identical(RSArray$new(list(raw_value))$compare(
    RSArray$new(list(raw_value))
  ), 0L)
})
