test_that("exported RObjC generators have class versions and identity records", {
  ns <- asNamespace("robjc")
  exported <- getNamespaceExports("robjc")
  classes <- lapply(exported, get, envir = ns)
  classes <- Filter(robjc_isMetaClass, classes)

  expect_gt(length(classes), 0)
  for (klass in classes) {
    expect_identical(klass$robjc_isa$class, klass)
    expect_identical(klass$private_fields$.__isa, klass$robjc_isa)
    expect_type(klass$robjc_isa$version, "character")
    expect_length(klass$robjc_isa$version, 1)
  }
})

test_that("RSObject respondsTo validates method names", {
  object <- RSObject$new()
  expect_true(object$respondsTo("hash"))
  expect_false(object$respondsTo("absentMethod"))

  invalid_names <- list(NULL, character(0), c("hash", "copy"),
                        NA_character_, "", 1L)
  for (invalid in invalid_names) {
    expect_error(
      object$respondsTo(invalid),
      "`method` must be one non-empty character string.",
      fixed = TRUE
    )
  }
})

test_that("class_getInstanceMethod validates names and searches ancestors", {
  get_method <- robjc:::class_getInstanceMethod
  expect_identical(get_method(RSNumber, "initialize"), RSNumber$public_methods$initialize)
  expect_identical(get_method(RSNumber, "respondsTo"), RSObject$public_methods$respondsTo)
  expect_null(get_method(RSNumber, "absentMethod"))

  invalid_names <- list(NULL, character(0), c("hash", "copy"),
                        NA_character_, "", 1L)
  for (invalid in invalid_names) {
    expect_error(
      get_method(RSNumber, invalid),
      "`method_name` must be one non-empty character string.",
      fixed = TRUE
    )
  }
})

test_that("robjc_inheritChain accepts identity records only", {
  Parent <- RSObjectClass("InheritChainParent")
  Child <- RSObjectClass("InheritChainChild", super_class = Parent)
  Protocol <- RSProtocolClass("InheritChainProtocol")

  expect_identical(robjc_inheritChain(Child$robjc_isa),
                   list(Child, Parent, RSObject))
  expect_identical(robjc_inheritChain(Protocol$robjc_isa),
                   list(Protocol, RSProtocol))

  for (invalid in list(NULL, list(), Child, 1L)) {
    expect_error(robjc_inheritChain(invalid),
                 "`isa` must be an RObjC identity record.", fixed = TRUE)
  }
})

test_that("RSObject respondsTo reports only public methods", {
  scalar <- RSNumber$new(1L)
  expect_true(scalar$respondsTo("isEqual"))
  expect_false(scalar$respondsTo(".comparable"))
})

test_that("robjc_isRobjcObject rejects malformed R6-like environments", {
  fake <- new.env(parent = emptyenv())
  fake$.__enclos_env__ <- 1L
  expect_false(robjc_isRobjcObject(fake))

  fake$.__enclos_env__ <- new.env(parent = emptyenv())
  fake$.__enclos_env__$private <- 1L
  expect_false(robjc_isRobjcObject(fake))

  fake$.__enclos_env__$private <- new.env(parent = emptyenv())
  fake$.__enclos_env__$private$.__isa <- structure(list(), class = "RSObjectIsA")
  expect_false(robjc_isRobjcObject(fake))
  expect_true(robjc_isRobjcObject(RSObject$new()))
})

test_that("resolveClassWith updates the S3 class hierarchy", {
  Parent <- RSObjectClass("ResolutionParent")
  Original <- RSObjectClass("ResolutionTarget", version = "1.0")
  Replacement <- RSObjectClass("ResolutionTarget", super_class = Parent,
                               version = "1.0")
  object <- Original$new()

  expect_false(inherits(object, "ResolutionParent"))
  object$resolveClassWith(Replacement)
  expect_identical(object$class(), Replacement)
  expect_identical(class(object), c("ResolutionTarget", "ResolutionParent", "RSObject"))
  expect_true(inherits(object, "ResolutionParent"))
  expect_true(object$isKindOf(Parent))
})

test_that("standard class discovery includes indirect RSObject subclasses", {
  classes <- get_standard_robjc_classes()
  expect_true(all(c("RSMutableArray", "RSArray", "RSSet", "RSVector") %in% classes))
  expect_false("RSObject" %in% classes)

  generators <- get_standard_robjc_classes(FALSE)
  expect_setequal(names(generators), classes)
  expect_identical(generators[["RSArray"]], RSArray)
  expect_identical(generators[["RSSet"]], RSSet)
})

test_that("RObjC generators have identity equality and pointer hashes", {
  protocol <- RSProtocolClass("MetaValueProtocol")
  first <- RSObjectClass("MetaValueSameName")
  second <- RSObjectClass("MetaValueSameName")
  generators <- list(RSObject, RSProtocol, RSCopying, RSString,
                     RSString$na$class(), RSClassRegistry, protocol, first)
  for (generator in generators) {
    expect_true(generator$isEqual(generator))
    expect_identical(generator$hash(), generator$hash())
    expect_type(generator$hash(), "integer")
    expect_length(generator$hash(), 1L)
    expect_false(RSString_withValue("value")$isEqual(generator))
  }
  expect_false(first$isEqual(second))
  expect_false(RSString$isEqual(RSNumber))
  expect_false(protocol$isEqual(RSProtocol))
})
