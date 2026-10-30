#' @title RSEncoderProtocol
#'
#' @description
#' Declares `encodeWithCoder(coder)` for objects that write their logical state
#' to a keyed coder. Conformance is a declaration; it does not check the method.
#'
#' @export
RSEncoderProtocol <- RSProtocolClass(
  "RSEncoderProtocol",
  version = "0.1.0 beta",
  contract_methods = "encodeWithCoder"
)




#' @title RSDecoderProtocol
#'
#' @description
#' Declares `initWithCoder(coder)` for objects that restore their logical state
#' from a keyed coder. Conformance is a declaration; it does not check the method.
#'
#' @export
RSDecoderProtocol <- RSProtocolClass(
  "RSDecoderProtocol",
  version = "0.1.0 beta",
  contract_methods = "initWithCoder"
)



#' @include api_setup.r
#' @title RSCoding: Encoding and Decoding Protocol
#' @description
#' Marker protocol for classes that support keyed encoding and decoding. The
#' method names are declared by [RSEncoderProtocol] and [RSDecoderProtocol].
#'
#' @export
RSCoding <- RSProtocolClass(
  "RSCoding",
  super_protocols = c(RSEncoderProtocol, RSDecoderProtocol),
  version = "0.1.0 beta"
)
