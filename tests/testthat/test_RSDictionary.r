test_that("RSDictionary: objectForKey returns correct value", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString_withValue("v1"), RSString_withValue("k1"),
    RSString_withValue("v2"), RSString_withValue("k2")
  ))
  v <- d$objectForKey(RSString_withValue("k1"))
  expect_s3_class(v, "RSString")
  expect_equal(v$value(), "v1")
})

test_that("RSDictionary: objectForKey returns NULL for missing key", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString_withValue("value"), RSString_withValue("key")
  ))
  expect_null(d$objectForKey(RSString_withValue("absent")))
})

test_that("RSDictionary: count returns correct size", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString_withValue("v1"), RSString_withValue("k1"),
    RSString_withValue("v2"), RSString_withValue("k2")
  ))
  expect_equal(d$count(), 2)
})

test_that("RSDictionary: mutation methods are prohibited", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString_withValue("v"), RSString_withValue("k")
  ))
  expect_error(
    d$setObjectForKey(RSString_withValue("v2"), RSString_withValue("k")),
    msg_immutable_try_mutate(d$class())
  )
  expect_error(
    d$removeObjectForKey(RSString_withValue("k")),
    msg_immutable_try_mutate(d$class())
  )
  expect_error(
    d$removeAllObjects(),
    msg_immutable_try_mutate(d$class())
  )
})

test_that("RSDictionary: copy returns identical", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString_withValue("v"), RSString_withValue("k")
  ))
  copy <- d$copy()
  expect_identical(d, copy)
})

test_that("RSDictionary: mutableCopy returns RSMutableDictionary", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString_withValue("v"), RSString_withValue("k")
  ))
  mcopy <- d$mutableCopy()
  expect_equal(mcopy$objectForKey(RSString_withValue("k"))$value(), "v")
})

test_that("RSDictionary: entry mutation aliases are prohibited", {
  key <- RSString_withValue("k")
  entry <- RSKeyValuePair$new(key, RSString_withValue("v"))
  dictionary <- RSDictionary$new(key_value_pairs = list(entry))
  expected <- msg_immutable_try_mutate(dictionary$class())

  expect_error(dictionary$addEntry(entry), expected, fixed = TRUE)
  expect_error(dictionary$removeEntry(entry), expected, fixed = TRUE)
  expect_error(dictionary$removeAllEntries(), expected, fixed = TRUE)
})

test_that("RSDictionary: dictionaryWithDictionary clones native storage", {
  key <- RSString_withValue("k")
  source <- RSMutableDictionary$new(
    keys = list(key), values = list(RSString_withValue("before"))
  )

  dictionary <- RSDictionary$dictionaryWithDictionary(source)
  source$setObjectForKey(RSString_withValue("after"), key)

  expect_true(dictionary$isMemberOf(RSDictionary))
  expect_equal(dictionary$objectForKey(key)$value(), "before")
})
