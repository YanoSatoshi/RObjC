test_that("storage conversion preserves empty collections as empty lists", {
  empty_collections <- list(
    RSMutableArray$new(),
    RSMutableSet$new(),
    RSMutableDictionary$new()
  )

  expect_identical(robjc:::cast_collections_to_list(list()), list())
  for (collection in empty_collections) {
    expect_identical(collection$asList(), list())
    expect_identical(robjc:::cast_collections_to_list(collection), list())
  }
})

test_that("storage conversion rejects RObjC objects without asList", {
  scalar <- RSString_withValue("x")
  expect_false(scalar$respondsTo("asList"))

  expect_error(
    robjc:::cast_collections_to_list(scalar),
    "`collections` must provide `asList()`", fixed = TRUE
  )
  expect_error(
    RSMutableSet$new(scalar),
    "`collections` must provide `asList()`", fixed = TRUE
  )
  expect_error(
    RSMutableDictionary$new(key_value_pairs = scalar),
    "`collections` must provide `asList()`", fixed = TRUE
  )
})

test_that("storage conversion rejects non-collections and non-list results", {
  for (input in list(NULL, 1L, "x", new.env(parent = emptyenv()))) {
    expect_error(
      robjc:::cast_collections_to_list(input),
      "`collections` must be a list or RObjC instance.", fixed = TRUE
    )
  }

  NullAsList <- RSObjectClass(
    "UtilityNullAsList",
    public = list(asList = function() NULL)
  )
  AtomicAsList <- RSObjectClass(
    "UtilityAtomicAsList",
    public = list(asList = function() 1L)
  )
  for (collection in list(NullAsList$new(), AtomicAsList$new())) {
    expect_error(
      robjc:::cast_collections_to_list(collection),
      "`asList()` must return a list.", fixed = TRUE
    )
    expect_error(
      RSMutableSet$new(collection),
      "`asList()` must return a list.", fixed = TRUE
    )
  }
})
