resolve_coding_collection_class <- function(class_name, version, kind) {
  expect_identical(version, "0.1.0 beta")
  expect_true(kind %in% c("instance", "class"))
  switch(class_name,
    RSMutableArray = RSMutableArray,
    RSMutableSet = RSMutableSet,
    RSSet = RSSet,
    RSMutableDictionary = RSMutableDictionary,
    RSDictionary = RSDictionary,
    RSNumber = RSNumber,
    RSString = RSString,
    RSNull = RSNull,
    RSFactor = RSFactor,
    RSNAFactor = RSNAFactor,
    stop("Unexpected class: ", class_name)
  )
}

round_trip_coding_collection <- function(object) {
  archive <- RSKeyedArchiver$new()$archive(object)
  RSKeyedUnarchiver$new(
    unserialize(serialize(archive, NULL)),
    resolve_coding_collection_class
  )$unarchive()
}

test_that("mutable and immutable sets restore members and shared references", {
  shared <- RSNumber$new(42L)
  for (set_class in list(RSMutableSet, RSSet)) {
    source <- set_class$new(list(shared, RSNull$null, RSString))
    restored <- round_trip_coding_collection(list(source, shared))
    set <- restored[[1L]]
    expect_true(set$isMemberOf(set_class))
    expect_equal(set$count(), 3)
    expect_identical(set$member(restored[[2L]]), restored[[2L]])
    expect_identical(set$member(RSNull$null), RSNull$null)
    expect_identical(set$member(RSString), RSString)
    if (identical(set_class, RSSet))
      expect_error(set$addObject(RSNumber$new(1L)), "immutable")
    else
      expect_identical(set$addObject(RSNumber$new(1L)), set)
  }
})

test_that("dictionaries restore keys, values, cycles and immutability", {
  for (dictionary_class in list(RSMutableDictionary, RSDictionary)) {
    source <- dictionary_class$new()
    key <- RSString$new("self")
    if (identical(dictionary_class, RSMutableDictionary)) {
      source$setObjectForKey(source, key)
      source$setObjectForKey(RSNull$null, RSNull$null)
      source$setObjectForKey(RSNumber$new(7L), RSString)
    } else {
      source <- RSDictionary$new(keys = list(key, RSNull$null, RSString),
                                 values = list(source, RSNull$null, RSNumber$new(7L)))
    }
    restored <- round_trip_coding_collection(source)
    expect_true(restored$isMemberOf(dictionary_class))
    expect_equal(restored$count(), 3)
    expect_identical(restored$objectForKey(RSNull$null), RSNull$null)
    expect_identical(restored$objectForKey(RSString)$value(), 7L)
    if (identical(dictionary_class, RSMutableDictionary)) {
      expect_identical(restored$objectForKey(key), restored)
    } else {
      expect_error(restored$setObjectForKey(RSNull$null, key), "immutable")
    }
  }
})

test_that("dictionary restores the stored key without copying it again", {
  CopyKey <- RSObjectClass("CopyKeyForCoding", version = "0.1.0 beta",
    public = list(
      initialize = function(value) {
        super$initialize()
        self$value <- value
        invisible(self)
      },
      value = NULL,
      copy = function() CopyKey$new(self$value),
      hash = function() robjc_normalize_hash(as.integer(self$value)),
      isEqual = function(other) robjc_isRobjcObject(other) &&
        other$isKindOf(CopyKey) && identical(self$value, other$value),
      encodeWithCoder = function(coder) coder$encodeObjectForKey(self$value, "value"),
      initWithCoder = function(coder) {
        self$value <- coder$decodeObjectForKey("value")
        invisible(self)
      }
    ))
  dictionary <- RSMutableDictionary$new()
  dictionary$setObjectForKey(RSNumber$new(1L), CopyKey$new(12L))
  stored_key <- dictionary$allKeysAsList()[[1L]]
  archive <- RSKeyedArchiver$new()$archive(list(dictionary, stored_key))
  restored <- RSKeyedUnarchiver$new(archive, function(name, version, kind) {
    if (identical(name, "CopyKeyForCoding")) CopyKey else
      resolve_coding_collection_class(name, version, kind)
  })$unarchive()
  expect_identical(restored[[1L]]$allKeysAsList()[[1L]], restored[[2L]])
})

test_that("factor metadata and null singleton survive keyed coding", {
  regular <- RSFactor$new(factor("low", levels = c("low", "high"), ordered = TRUE))
  missing <- RSNAFactor$new(factor(NA_character_, levels = c("a", "b")))
  restored <- round_trip_coding_collection(list(regular, missing, RSNull$null, RSNull$null))
  expect_identical(restored[[1L]]$asFactor(), regular$asFactor())
  expect_identical(restored[[2L]]$asFactor(), missing$asFactor())
  expect_identical(restored[[3L]], RSNull$null)
  expect_identical(restored[[3L]], restored[[4L]])
})

test_that("scalar subclasses retain native values and NA companions", {
  values <- list(
    RSBoolean$new(TRUE),
    RSNumber$new(NaN),
    RSString$new("text"),
    RSComplex$new(2 + 3i),
    RSRaw$new(as.raw(42)),
    RSDate$new(as.Date("2026-10-30")),
    RSDateTime$new(as.POSIXct("2026-10-30 10:00:00", tz = "Asia/Tokyo")),
    RSBoolean$na,
    RSNumber$na,
    RSString$na,
    RSComplex$na,
    RSDate$na,
    RSDateTime$na
  )
  generators <- list(
    RSBoolean = RSBoolean, RSNumber = RSNumber, RSString = RSString,
    RSComplex = RSComplex, RSRaw = RSRaw, RSDate = RSDate,
    RSDateTime = RSDateTime, RSNABoolean = RSNABoolean,
    RSNANumber = RSNANumber, RSNAString = RSNAString,
    RSNAComplex = RSNAComplex, RSNADate = RSNADate,
    RSNADateTime = RSNADateTime
  )
  archive <- RSKeyedArchiver$new()$archive(values)
  restored <- RSKeyedUnarchiver$new(
    unserialize(serialize(archive, NULL)),
    function(name, version, kind) generators[[name]]
  )$unarchive()
  for (i in seq_along(values)) {
    expect_identical(restored[[i]]$class(), values[[i]]$class())
    expect_identical(restored[[i]]$value(), values[[i]]$value())
  }
  for (i in 8:13) expect_identical(restored[[i]], values[[i]])
})
