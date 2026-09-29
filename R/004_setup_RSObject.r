## Manually class setting ##



stopifnot(is.null(RSObject$robjc_isa))
RSObject$robjc_isa$inherit_chain <- list(RSObject)
RSObject$robjc_isa$protocols <- setNames(
  list(RSEncoding, RSCopying),
  c(RSEncoding$classname, RSCopying$classname)
)




register_rs_protocol(RSProtocol, RSProtocol$new())
stopifnot(!is.null(RSProtocol$robjc_isa$protocols))


register_rs_protocol(RSEncoderProtocol,       RSEncoderProtocol$new())
register_rs_protocol(RSDecoderProtocol,       RSDecoderProtocol$new())
register_rs_protocol(RSHashOperatorProtocol,  RSHashOperatorProtocol$new())


register_rs_protocol(RSEncoding,              RSEncoding$new())
register_rs_protocol(RSCopying,               RSCopying$new())
register_rs_protocol(RSMutableCopying,        RSMutableCopying$new())


register_rs_singleton(RSSingleton, RSSingleton$new())
