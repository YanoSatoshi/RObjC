test_that("RSDictionary: objectForKey returns correct value", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString$stringWith("v1"), RSString$stringWith("k1"),
    RSString$stringWith("v2"), RSString$stringWith("k2")
  ))
  v <- d$objectForKey(RSString$stringWith("k1"))
  expect_s3_class(v, "RSString")
  expect_equal(v$value(), "v1")
})

test_that("RSDictionary: objectForKey returns NULL for missing key", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString$stringWith("value"), RSString$stringWith("key")
  ))
  expect_null(d$objectForKey(RSString$stringWith("absent")))
})

test_that("RSDictionary: count returns correct size", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString$stringWith("v1"), RSString$stringWith("k1"),
    RSString$stringWith("v2"), RSString$stringWith("k2")
  ))
  expect_equal(d$count(), 2)
})

test_that("RSDictionary: mutation methods are prohibited", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString$stringWith("v"), RSString$stringWith("k")
  ))
  expect_error(
    d$setObjectForKey(RSString$stringWith("v2"), RSString$stringWith("k")),
    msg_immutable_try_mutate(d$class())
  )
  expect_error(
    d$removeObjectForKey(RSString$stringWith("k")),
    msg_immutable_try_mutate(d$class())
  )
  expect_error(
    d$removeAllObjects(),
    msg_immutable_try_mutate(d$class())
  )
})

test_that("RSDictionary: copy returns identical", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString$stringWith("v"), RSString$stringWith("k")
  ))
  copy <- d$copy()
  expect_identical(d, copy)
})

test_that("RSDictionary: mutableCopy returns RSMutableDictionary", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString$stringWith("v"), RSString$stringWith("k")
  ))
  mcopy <- d$mutableCopy()
  expect_equal(mcopy$objectForKey(RSString$stringWith("k"))$value(), "v")
})

test_that("RSDictionary: entry mutation aliases are prohibited", {
  key <- RSString$stringWith("k")
  entry <- RSKeyValuePair$new(key, RSString$stringWith("v"))
  dictionary <- RSDictionary$new(key_value_pairs = list(entry))
  expected <- msg_immutable_try_mutate(dictionary$class())

  expect_error(dictionary$addEntry(entry), expected, fixed = TRUE)
  expect_error(dictionary$removeEntry(entry), expected, fixed = TRUE)
  expect_error(dictionary$removeAllEntries(), expected, fixed = TRUE)
})

test_that("RSDictionary: dictionaryWithDictionary clones native storage", {
  key <- RSString$stringWith("k")
  source <- RSMutableDictionary$new(
    keys = list(key), values = list(RSString$stringWith("before"))
  )

  dictionary <- RSDictionary$dictionaryWithDictionary(source)
  source$setObjectForKey(RSString$stringWith("after"), key)

  expect_true(dictionary$isMemberOf(RSDictionary))
  expect_equal(dictionary$objectForKey(key)$value(), "before")
})
