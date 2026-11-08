test_that("RSArray: copy returns identical copy", {
  ary <- RSArray$new(list(
    RSString_withValue("a"),
    RSString_withValue("b")
  ))
  copied <- ary$copy()
  expect_true(identical(ary, copied))
})

test_that("RSArray: mutableCopy returns RSMutableArray", {
  ary <- RSArray$new(list(RSString_withValue("x")))
  mcopy <- ary$mutableCopy()
  expect_equal(mcopy$objectAtIndex(1)$value(), "x")
})

test_that("objectsAtIndexes returns an immutable array in requested order", {
  first <- RSString_withValue("a")
  second <- RSString_withValue("b")
  source <- RSMutableArray$new(list(first, second))
  selected <- source$objectsAtIndexes(c(2.9, 1, 2))

  expect_identical(selected$class(), RSArray)
  expect_identical(selected$asList(), list(second, first, second))
  expect_identical(source$asList(), list(first, second))
  expect_identical(source$objectsAtIndexes(numeric())$asList(), list())
  expect_identical(RSArray$new()$objectsAtIndexes(integer())$asList(), list())

  for (indexes in list(0.9, 3, NA_real_, Inf, "1", list(1), 2^31))
    expect_error(source$objectsAtIndexes(indexes), "Index out of bounds", fixed = TRUE)
  expect_error(source$objectsAtIndexes(c(2, 0, 3)),
               "Index out of bounds: 0, 3.", fixed = TRUE)
  expect_error(source$objectAtIndex(c(1, 2)),
               "Index out of bounds: c(1, 2).", fixed = TRUE)
})

test_that("Array derivation retains references and order", {
  first <- RSString_withValue("a")
  second <- RSString_withValue("b")
  source <- RSMutableArray$new(list(first))
  other <- RSArray$new(list(second, first))

  expect_identical(source$lastObject(), first)
  expect_error(RSArray$new()$lastObject(), "Index out of bounds", fixed = TRUE)

  added <- source$arrayByAddingObject(second)
  combined <- source$arrayByAddingObjectsFromArray(other)
  filtered <- combined$filteredArrayByFunction(function(obj) identical(obj, first))
  for (array in list(added, combined, filtered)) {
    expect_identical(array$class(), RSArray)
  }
  expect_identical(added$asList(), list(first, second))
  expect_identical(combined$asList(), list(first, second, first))
  expect_identical(filtered$asList(), list(first, first))
  expect_identical(source$asList(), list(first))
  expect_identical(other$asList(), list(second, first))
  expect_identical(source$arrayByAddingObjectsFromArray(
    RSArray$new())$asList(),
                   list(first))
  expect_identical(source$filteredArrayByFunction(function(obj) FALSE)$asList(), list())
})

test_that("Array derivation rejects unsupported inputs and predicates", {
  source <- RSArray$new(list(RSString_withValue("a")))
  expect_error(source$arrayByAddingObject(NULL), "Element must be", fixed = TRUE)
  for (other in list(NULL, list(), RSObject$new()))
    expect_error(source$arrayByAddingObjectsFromArray(other),
                 "`other` must be a kind of RSArray.", fixed = TRUE)
  expect_error(source$filteredArrayByFunction(NULL), "`lambda` must be a function.", fixed = TRUE)
  for (predicate in list(function(x) NA, function(x) TRUE[FALSE],
                         function(x) c(TRUE, FALSE), function(x) 1L))
    expect_error(source$filteredArrayByFunction(predicate),
                 "`lambda` must return one non-missing logical value.", fixed = TRUE)
})

test_that("RSArray: immutable API has no mutation methods", {
  ary <- RSArray$new(list(RSString_withValue("x")))
  for (method in c("addObject", "insertObjectAtIndex", "replaceObjectAtIndex",
                   "removeObjectAtIndex", "removeAllObjects")) {
    expect_false(ary$respondsTo(method))
  }
  expect_false(ary$isKindOf(RSMutableArray))
  mutable <- ary$mutableCopy()
  expect_true(mutable$isKindOf(RSArray))
  expect_true(mutable$respondsTo("addObject"))
})

test_that("RSArray: mutable copies share elements but not list storage", {
  element <- RSString_withValue("same")
  original <- RSArray$new(list(element))
  mutable <- original$mutableCopy()
  expect_identical(original$allObjects(), original)
  expect_identical(mutable$allObjects()$objectAtIndex(1L), element)
  expect_false(identical(mutable$allObjects(), mutable))
  mutable$replaceObjectAtIndex(RSString_withValue("changed"), 1L)
  expect_identical(original$objectAtIndex(1L), element)
  expect_identical(mutable$objectAtIndex(1L)$value(), "changed")
})
