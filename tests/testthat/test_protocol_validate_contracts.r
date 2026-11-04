test_that("protocol_validateContracts reports missing inherited requirements", {
  Named <- RSProtocolClass("ValidateContractNamed", contract_methods = "name")
  Detailed <- RSProtocolClass(
    "ValidateContractDetailed",
    super_protocols = list(Named),
    contract_methods = c("details", "code")
  )
  Parent <- RSObjectClass(
    "ValidateContractParent",
    public = list(name = function() "parent")
  )
  Child <- RSObjectClass(
    "ValidateContractChild",
    super_class = Parent,
    protocols = list(Detailed),
    public = list(details = function() "child"),
    validate_protocols = FALSE
  )
  Complete <- RSObjectClass(
    "ValidateContractComplete",
    super_class = Child,
    public = list(code = function() "ok")
  )

  expect_identical(protocol_validateContracts(Child), "code")
  expect_identical(protocol_validateContracts(Complete), character(0))
  expect_identical(protocol_validateContracts(Parent), character(0))
})

test_that("protocol_validateContracts validates its inputs", {
  Example <- RSObjectClass("ValidateContractExample")
  expect_error(protocol_validateContracts(1L),
               "`klass` must be an RObjC object class generator.", fixed = TRUE)
  expect_error(protocol_validateContracts(RSProtocol),
               "`klass` must be an RObjC object class generator.", fixed = TRUE)
  expect_identical(protocol_validateContracts(Example), character(0))
})
