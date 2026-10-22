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
  expect_equal(obj$class()$robjc_isa$version, "2.1")
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
  expect_null(Parent$new)
  expect_null(Child$new)
  expect_true(bindingIsLocked("new", Parent))
  expect_true(bindingIsLocked("new", Child))
  expect_error(Parent$new(), "attempt to apply non-function", fixed = TRUE)
  expect_error(Child$new(), "attempt to apply non-function", fixed = TRUE)
})

test_that("root and directly declared protocols cannot create instances", {
  for (protocol in list(RSProtocol, RSCopying, RSComparable)) {
    expect_true(robjc_isProtocol(protocol))
    expect_null(protocol$new)
    expect_true(bindingIsLocked("new", protocol))
    expect_error(protocol$new(), "attempt to apply non-function", fixed = TRUE)
  }
})

test_that("instances inherit protocol conformance from parent classes", {
  key <- RSString_withValue("key")
  value <- RSString_withValue("value")

  expect_true(key$conformsTo(RSCopying))
  expect_equal(RSDictionary_withObjectForKey(value, key)$count(), 1)
})

test_that("RSSingletonClass creates and locks its shared instance", {
  Shared <- RSSingletonClass("SharedWidget", version = "0.0.4 alpha")

  expect_identical(Shared$instance$class(), Shared)
  expect_identical(Shared$instance, Shared$robjc_isa$slot)
  expect_true(bindingIsLocked("instance", Shared))
})

test_that("singleton identity helper returns the identity it creates", {
  generator <- R6::R6Class(
    "AuditSingletonIdentity",
    inherit = RSObject,
    class = FALSE,
    cloneable = FALSE
  )

  created_isa <- robjc:::robjc_make_singleton_isa(generator, RSObject)
  expect_identical(created_isa, generator$robjc_isa)
  expect_identical(generator$instance$class(), generator)
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

test_that("RSScalarClass rejects a dimensioned NA value before class creation", {
  expect_error(
    RSScalarClass("DimensionedNAClass", na_value = matrix(NA_integer_, 1L, 1L)),
    "`na_value` must be a scalar NA.", fixed = TRUE
  )
})

test_that("RSScalarClass accepts scalar POSIXlt NA like RSScalar", {
  LtValue <- RSScalarClass(
    "AuditPOSIXltNAValue",
    na_value = as.POSIXlt(NA),
    public = list(initialize = function(val) {
      super$initialize(val)
      invisible(self)
    })
  )

  expect_true(LtValue$na$isKindOf(LtValue))
  expect_true(inherits(LtValue$na$value(), "POSIXlt"))
  expect_true(is.na(LtValue$na$value()))
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
  expect_true(robjc:::class_isSingleton(RSNANumber))
  expect_false(robjc:::class_isSingleton(RSNAFactor))
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

test_that("class and protocol factories reject empty or missing names", {
  factories <- list(RSObjectClass, RSSingletonClass, RSProtocolClass)
  for (factory in factories) {
    expect_error(factory(""), "must be one nonempty character string", fixed = TRUE)
    expect_error(factory(NA_character_), "must be one nonempty character string", fixed = TRUE)
  }
})

test_that("RSProtocolClass rejects empty or missing contract method names", {
  expect_error(
    RSProtocolClass("EmptyContractMethod", contract_methods = ""),
    "must contain nonempty character names", fixed = TRUE
  )
  expect_error(
    RSProtocolClass("MissingContractMethod", contract_methods = c("ok", NA_character_)),
    "must contain nonempty character names", fixed = TRUE
  )
  expect_identical(
    robjc:::protocol_getAllContracts(RSProtocolClass("NoContractMethods")),
    character(0)
  )
})

test_that("class factories reject invalid identity metadata before creation", {
  factories <- list(
    object = function(protocols, version) {
      RSObjectClass("InvalidObjectMetadata", protocols = protocols, version = version)
    },
    singleton = function(protocols, version) {
      RSSingletonClass("InvalidSingletonMetadata", protocols = protocols,
                       version = version)
    },
    scalar = function(protocols, version) {
      RSScalarClass("InvalidScalarMetadata", protocols = protocols,
                    version = version)
    },
    protocol = function(protocols, version) {
      RSProtocolClass("InvalidProtocolMetadata", super_protocols = protocols,
                      version = version)
    }
  )

  for (factory in factories) {
    expect_error(factory(list(1L), "1.0"),
                 "must be a list of RObjC protocols", fixed = TRUE)
    expect_error(factory(1L, "1.0"),
                 "must be a list of RObjC protocols", fixed = TRUE)
    for (invalid_version in list(NA_character_, "", 1L, character(0))) {
      expect_error(factory(list(), invalid_version),
                   "`version` must be one non-empty character string.",
                   fixed = TRUE)
    }
  }
})
