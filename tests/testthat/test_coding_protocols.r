test_that("built-in keyed coding classes declare both protocol contracts", {
  instances <- list(
    RSMutableArray$new(), RSArray$new(),
    RSMutableSet$new(), RSSet$new(),
    RSMutableDictionary$new(), RSDictionary$new(),
    RSMutableVector$new(integer()), RSVector$new(integer()),
    RSMutableDataFrame$new(), RSDataFrame$new(),
    RSKeyValuePair$new(RSNull$null, RSNull$null),
    RSBoolean$new(TRUE), RSNumber$new(1L), RSString$new("x"),
    RSFactor$new(factor("x")), RSDate$new(as.Date("2026-10-30")),
    RSDateTime$new(as.POSIXct("2026-10-30", tz = "UTC")),
    RSComplex$new(1 + 2i), RSRaw$new(as.raw(1)), RSNull$null,
    RSNumber$na, RSFactor$na
  )
  for (object in instances) {
    expect_true(object$conformsTo(RSCoding), info = object$class()$classname)
    expect_true(object$conformsTo(RSEncoderProtocol), info = object$class()$classname)
    expect_true(object$conformsTo(RSDecoderProtocol), info = object$class()$classname)
    expect_true(object$respondsTo("encodeWithCoder"), info = object$class()$classname)
    expect_true(object$respondsTo("initWithCoder"), info = object$class()$classname)
  }
  expect_false(RSObject$new()$conformsTo(RSCoding))
  expect_false(RSHashOperator$instance$conformsTo(RSCoding))
})

test_that("child identity records directly list inherited protocols", {
  namespace <- asNamespace("robjc")
  for (name in ls(namespace, all.names = TRUE)) {
    klass <- get(name, envir = namespace, inherits = FALSE)
    if (!robjc_isClass(klass) || is.null(klass$robjc_isa$super)) next
    direct <- klass$robjc_isa$protocols
    ancestors <- robjc_inheritChain(klass$robjc_isa$super$robjc_isa)
    for (ancestor in ancestors) {
      for (protocol in ancestor$robjc_isa$protocols) {
        expect_true(any(vapply(direct, identical, logical(1), protocol)),
                    info = paste(name, protocol$classname))
      }
    }
  }

  Extra <- RSProtocolClass("InheritedProtocolAudit")
  Parent <- RSObjectClass("InheritedProtocolParent", protocols = list(Extra))
  Child <- RSObjectClass("InheritedProtocolChild", super_class = Parent,
                         protocols = list(Extra))
  direct <- Child$robjc_isa$protocols
  expect_identical(length(direct), 2L)
  expect_true(identical(direct[[1L]], Extra))
  expect_true(identical(direct[[2L]], RSCopying))
  expect_true(any(vapply(RSNANumber$robjc_isa$protocols,
                         identical, logical(1), RSCoding)))
})
