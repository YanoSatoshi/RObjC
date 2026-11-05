test_that("class factories reject missing protocol methods by default", {
  Required <- RSProtocolClass(
    "FactoryValidationRequired",
    contract_methods = "requiredMethod"
  )

  expect_error(
    RSObjectClass("FactoryValidationObject", protocols = list(Required)),
    "Class `FactoryValidationObject` is missing protocol methods: requiredMethod.",
    fixed = TRUE
  )

  initializations <- 0L
  expect_error(
    RSSingletonClass(
      "FactoryValidationSingleton",
      protocols = list(Required),
      public = list(initialize = function() {
        initializations <<- initializations + 1L
        super$initialize()
      })
    ),
    "Class `FactoryValidationSingleton` is missing protocol methods: requiredMethod.",
    fixed = TRUE
  )
  expect_identical(initializations, 0L)

  expect_error(
    RSScalarClass(
      "FactoryValidationScalar",
      protocols = list(Required),
      public = list(initialize = function(val) super$initialize(val))
    ),
    "Class `FactoryValidationScalar` is missing protocol methods: requiredMethod.",
    fixed = TRUE
  )
})

test_that("validation accepts inherited methods and can be deferred", {
  Named <- RSProtocolClass("FactoryValidationNamed", contract_methods = "name")
  Parent <- RSObjectClass(
    "FactoryValidationParent",
    public = list(name = function() "parent")
  )
  Child <- RSObjectClass(
    "FactoryValidationChild",
    super_class = Parent,
    protocols = list(Named)
  )
  expect_identical(protocol_validateContracts(Child), character(0))

  Object <- RSObjectClass(
    "FactoryValidationDeferredObject",
    protocols = list(Named),
    validate_protocols = FALSE
  )
  Singleton <- RSSingletonClass(
    "FactoryValidationDeferredSingleton",
    protocols = list(Named),
    validate_protocols = FALSE
  )
  Scalar <- RSScalarClass(
    "FactoryValidationDeferredScalar",
    protocols = list(Named),
    public = list(initialize = function(val) super$initialize(val)),
    validate_protocols = FALSE
  )
  for (klass in list(Object, Singleton, Scalar, Scalar$na$class()))
    expect_identical(protocol_validateContracts(klass), "name")
})

test_that("class factories validate the validation flag", {
  for (invalid in list(NULL, NA, 1L, logical(), c(TRUE, FALSE))) {
    expect_error(RSObjectClass("FactoryValidationBadObject", validate_protocols = invalid),
                 "`validate_protocols` must be TRUE or FALSE.", fixed = TRUE)
    expect_error(RSSingletonClass("FactoryValidationBadSingleton", validate_protocols = invalid),
                 "`validate_protocols` must be TRUE or FALSE.", fixed = TRUE)
    expect_error(RSScalarClass("FactoryValidationBadScalar", validate_protocols = invalid),
                 "`validate_protocols` must be TRUE or FALSE.", fixed = TRUE)
  }
})
