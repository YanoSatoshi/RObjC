test_that("key-value pairs restore shared key and value references", {
  shared <- RSString$new("same")
  pair <- RSKeyValuePair$new(shared, shared)
  archive <- RSKeyedArchiver$new()$archive(list(pair, pair, shared))
  restored <- RSKeyedUnarchiver$new(
    unserialize(serialize(archive, NULL)),
    function(name, version, kind) switch(name,
      RSKeyValuePair = RSKeyValuePair,
      RSString = RSString,
      stop("Unexpected class: ", name)
    )
  )$unarchive()
  expect_true(restored[[1L]]$isMemberOf(RSKeyValuePair))
  expect_identical(restored[[1L]], restored[[2L]])
  expect_identical(restored[[1L]]$key(), restored[[3L]])
  expect_identical(restored[[1L]]$value(), restored[[3L]])
  expect_identical(restored[[1L]]$hash(), restored[[3L]]$hash())
})

test_that("key-value pairs restore class and protocol generators", {
  protocol <- RSProtocolClass("PairCodingProtocol", version = "1.0.0")
  pair <- RSKeyValuePair$new(protocol, RSNumber)
  archive <- RSKeyedArchiver$new()$archive(pair)
  restored <- RSKeyedUnarchiver$new(archive,
    function(name, version, kind) switch(name,
      RSKeyValuePair = RSKeyValuePair,
      RSNumber = RSNumber,
      PairCodingProtocol = protocol,
      stop("Unexpected class: ", name)
    ))$unarchive()
  expect_identical(restored$key(), protocol)
  expect_identical(restored$value(), RSNumber)
})

test_that("key-value pair decoding rejects an invalid key", {
  pair <- RSKeyValuePair$new(RSString$new("key"), RSNull$null)
  archive <- RSKeyedArchiver$new()$archive(pair)
  archive$records[[robjc_get_pointer(pair)]]$fields$key <-
    list(type = "r", value = 42L)
  expect_error(RSKeyedUnarchiver$new(archive,
    function(name, version, kind) switch(name,
      RSKeyValuePair = RSKeyValuePair,
      RSNull = RSNull,
      RSString = RSString,
      stop("Unexpected class: ", name)
    ))$unarchive(), "Decoded key must be an RObjC object")
})
