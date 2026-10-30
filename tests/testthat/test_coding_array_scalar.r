test_that("mutable arrays and scalars round-trip with shared references", {
  shared <- RSNumber$new(42L)
  source <- RSMutableArray$new(list(shared, shared, RSNumber$na))
  archive <- RSKeyedArchiver$new()$archive(source)

  decoder <- RSKeyedUnarchiver$new(unserialize(serialize(archive, NULL)))
  classes <- decoder$showAllClasses(remove_default = FALSE)
  expect_setequal(classes$class_name, c("RSMutableArray", "RSNumber", "RSNANumber"))
  expect_true(all(classes$status == "automatic"))
  restored <- decoder$decode()

  expect_true(restored$isMemberOf(RSMutableArray))
  expect_identical(restored$count(), 3L)
  expect_identical(restored$objectAtIndex(1L), restored$objectAtIndex(2L))
  expect_identical(restored$objectAtIndex(1L)$value(), 42L)
  expect_identical(restored$objectAtIndex(3L), RSNumber$na)
  expect_identical(restored$hashOperator(), RSHashOperator$instance)
  expect_false(identical(restored, source))
})

test_that("immutable arrays inherit keyed coding and remain immutable", {
  source <- RSArray$new(list(RSNumber$new(NaN)))
  archive <- RSKeyedArchiver$new()$archive(source)
  decoder <- RSKeyedUnarchiver$new(archive)
  expect_setequal(decoder$showAllClasses(remove_default = FALSE)$class_name,
                  c("RSArray", "RSNumber"))
  restored <- decoder$decode()

  expect_true(restored$isMemberOf(RSArray))
  expect_true(is.nan(restored$objectAtIndex(1L)$value()))
  expect_error(restored$addObject(RSNumber$new(1L)), "immutable")
})

test_that("mutable arrays can contain a reference to themselves", {
  source <- RSMutableArray$new()
  source$addObject(source)
  archive <- RSKeyedArchiver$new()$archive(source)
  decoder <- RSKeyedUnarchiver$new(archive)
  expect_identical(decoder$showAllClasses(remove_default = FALSE)$class_name,
                   "RSMutableArray")
  restored <- decoder$decode()
  expect_identical(restored$objectAtIndex(1L), restored)
})
