#' @include api_setup.r
#' @title RSEncoding: Protocol for Structural Encoding
#' @description
#' The `RSEncoding` protocol defines the requirements for any object that supports
#' structural encoding with [RSClassEncoder] and decoding with [RSClassDecoder].
#' Conforming classes must explicitly
#' declare which child objects should be recursively encoded, and must support
#' class substitution during decoding.
#'
#' This protocol is implemented by all robjc classes that need to be serialized.
#' It requires the presence of two methods: `objectsForEncoding()` and
#' `resolveClassWith(new_cls)`.
#'
#' @details
#' - `objectsForEncoding()`: Returns a flat list of direct child robjc objects.
#'   The encoder registers the current object separately. Overrides should
#'   include `super$objectsForEncoding()` and omit `NULL` and ordinary R values.
#' - `resolveClassWith(new_cls)`: Called during decoding to replace the object's
#'   class reference (`.__isa`) with the actual runtime class.
#'
#' @export
RSEncoding <- RSProtocolClass(
  "RSEncoding",
  contract_methods = c("objectsForEncoding", "resolveClassWith"),
  version = "0.0.4 alpha"
)





#' @title RSEncoderProtocol
#'
#' @description
#' This protocol defines the required methods for classes that perform
#' object encoding in the robjc system. An encoder must implement object
#' traversal and serialization logic, and produce metadata structures
#' that can be passed to the decoder.
#'
#' The typical implementer of this protocol is `RSClassEncoder`.
#'
#' @details
#' Required methods:
#' - `encodeObject(obj)`: Encodes the object and its children.
#' - `lockInstance()`: Finalizes and returns the complete encoded metadata.
#'
#' Optional methods may include:
#' - `encodingContainer(obj)`: Retrieves the class-bound instance storage.
#'
#' @export
RSEncoderProtocol <- RSProtocolClass(
  "RSEncoderProtocol",
  contract_methods = c("encodeObject", "lockInstance"),
  version = "0.0.4 alpha"
)




#' @title RSDecoderProtocol
#'
#' @description
#' This protocol defines the interface for any robjc-compatible decoder class.
#' A decoder is responsible for resolving saved class metadata and restoring
#' class relationships for previously encoded objects.
#'
#' The default implementation is `RSClassDecoder`, which performs full structure
#' resolution including class inheritance chains and object identity.
#'
#' @details
#' Required methods:
#' - `encodedClasses()`: Returns a list of [RSKeyValuePair] objects. Each key
#'   is an [RSString] class ID and each value is an [RSInstanceMap].
#' - `resolveClassesWithDictionary(class_map)`: Replaces placeholder class definitions
#'   with actual runtime classes.
#'
#' @export
RSDecoderProtocol <- RSProtocolClass(
  "RSDecoderProtocol",
  contract_methods = c("encodedClasses", "resolveClassesWithDictionary"),
  version = "0.0.4 alpha"
)
