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

test_that("RSScalarClass creates a shared NA companion", {
  One <- RSScalarClass(
    "OneScalar",
    na_value = NA_integer_,
    public = list(initialize = function(val) {
      super$initialize(val)
      invisible(self)
    })
  )

  expect_false(One$isNAClass)
  expect_true(One$na$class()$isNAClass)
  expect_identical(One$na, One$robjc_isa$slot)
  expect_identical(One$na, One$na$class()$instance)
  expect_identical(One$new(1L)$value(), 1L)
  expect_identical(One$na$value(), NA_integer_)
  expect_true(bindingIsLocked("isNAClass", One))
  expect_true(bindingIsLocked("na", One))
  expect_true(bindingIsLocked("instance", One$na$class()))
})

test_that("built-in scalar classes share their typed NA companions", {
  pairs <- list(
    list(RSBoolean, RSNABoolean, NA),
    list(RSNumber, RSNANumber, NA_real_),
    list(RSString, RSNAString, NA_character_),
    list(RSComplex, RSNAComplex, NA_complex_),
    list(RSFactor, RSNAFactor, factor(NA_character_)),
    list(RSDate, RSNADate, as.Date(NA)),
    list(RSDateTime, RSNADateTime, as.POSIXct(NA))
  )

  for (pair in pairs) {
    regular <- pair[[1L]]
    na_class <- pair[[2L]]
    na_value <- pair[[3L]]
    expect_identical(regular$na$class(), na_class)
    expect_identical(regular$na, regular$robjc_isa$slot)
    expect_identical(regular$na, na_class$instance)
    expect_identical(regular$na, na_class$robjc_isa$slot)
    expect_error(regular$new(na_value), na_class$classname, fixed = TRUE)
    expect_true(is.na(regular$na$value()))
    expect_identical(class(regular$na$value()),
                     if (is.factor(na_value)) "character" else class(na_value))
  }
})

test_that("date scalar initializers require an explicit value", {
  expect_error(RSDate$new(), "argument.*val.*missing")
  expect_error(RSDateTime$new(), "argument.*val.*missing")
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
    RSScalarClass("BadScalarVersion", na_value = NA_integer_, version = "1_0"),
    "Version string must not contain '_'",
    fixed = TRUE
  )
})
