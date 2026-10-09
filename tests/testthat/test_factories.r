test_that("RSObjectClass attaches class identity before instances are created", {
  Widget <- RSObjectClass(
    "Widget",
    version = "2.1",
    public = list(label = function() "widget")
  )
  obj <- Widget$new()

  expect_true(robjc_isClass(Widget))
  expect_identical(obj$class(), Widget)
  expect_true(obj$isKindOf(RSObject))
  expect_equal(obj$classVersion(), "2.1")
  expect_equal(obj$label(), "widget")
  expect_identical(Widget$private_fields$.__isa, Widget$robjc_isa)
  expect_true(bindingIsLocked("robjc_isa", Widget))
})

test_that("RSProtocolClass stores only newly declared requirements", {
  Parent <- RSProtocolClass("ParentProtocol", contract_methods = "foo")
  Child <- RSProtocolClass(
    "ChildProtocol",
    super_protocols = list(Parent),
    contract_methods = c("foo", "bar")
  )

  expect_true(robjc_isProtocol(Child))
  expect_identical(robjc:::protocol_getContract(Child), "bar")
  expect_setequal(robjc:::protocol_getAllContracts(Child), c("foo", "bar"))
  expect_true(robjc:::protocol_conformsTo(Child, Parent))
})

test_that("instances inherit protocol conformance from parent classes", {
  key <- RSString_withValue("key")
  value <- RSString_withValue("value")

  expect_true(key$conformsTo(RSCopying))
  expect_equal(RSDictionary_withObjectForKey(value, key)$count(), 1)
})

test_that("RSSingletonClass creates and locks its shared instance", {
  Shared <- RSSingletonClass("SharedWidget", version = "0.0.2 alpha")

  expect_identical(Shared$instance$class(), Shared)
  expect_identical(Shared$instance, Shared$robjc_isa$slot)
  expect_true(bindingIsLocked("instance", Shared))
})

test_that("RSScalarClass sets isNAClass before singleton initialization", {
  One <- RSScalarClass(
    "OneScalar",
    is_na_class = FALSE,
    singleton = TRUE,
    public = list(initialize = function() {
      super$initialize(1)
      invisible(self)
    })
  )

  expect_false(One$isNAClass)
  expect_equal(One$instance$value(), 1)
  expect_true(bindingIsLocked("isNAClass", One))
  expect_true(bindingIsLocked("instance", One))
})

test_that("factories reject reserved underscores in versions", {
  expect_error(
    RSObjectClass("BadObjectVersion", version = "1_0"),
    "Version string must not contain '_'",
    fixed = TRUE
  )
  expect_error(
    RSProtocolClass("BadProtocolVersion", version = "1_0"),
    "Version string must not contain '_'",
    fixed = TRUE
  )
  expect_error(
    RSSingletonClass("BadSingletonVersion", version = "1_0"),
    "Version string must not contain '_'",
    fixed = TRUE
  )
  expect_error(
    RSScalarClass("BadScalarVersion", is_na_class = FALSE, version = "1_0"),
    "Version string must not contain '_'",
    fixed = TRUE
  )
})
