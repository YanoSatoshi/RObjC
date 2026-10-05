#' @title RSEncoding Protocol
#'
#' @description
#' The `RSEncoding` protocol defines the requirements for any object that supports
#' structural encoding via the `RSClassEncoder` with `RSEncodingProtocol` and
#' `RSClassDecoder` with `RSDecodingProtocol`. Conforming classes must explicitly
#' declare which child objects should be recursively encoded, and must support
#' class substitution during decoding.
#'
#' This protocol is implemented by all robjc classes that need to be serialized.
#' It requires the presence of two methods: `objectsForEncoding()` and
#' `resolveClassWith(new_cls)`.
#'
#' @details
#' - `objectsForEncoding()`: Returns a list of robjc objects to be encoded
#'   along with the object itself.
#' - `resolveClassWith(new_cls)`: Called during decoding to replace the object's
#'   class reference (`.__isa`) with the actual runtime class.
#'
#' @export
RSEncoding <- R6::R6Class(
  "RSEncoding",
  inherit = RSProtocol,
  cloneable = FALSE,
  public = list(
    #' @description
    #' Lists the method names required by the `RSEncoding` protocol.
    #' 
    #' Conforming classes must implement both:
    #' - `objectsForEncoding()`: defines the child objects to encode
    #' - `resolveClassWith(new_cls)`: enables class reassignment on decoding
    #'
    #' @return A character vector of required method names.
    requiredMethods = function() {
      c("objectsForEncoding", "resolveClassWith")
    }
    # objectsForEncoding = function(void) {},
    # resolveClassWith = function(counter_class) {}
  )
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
RSEncoderProtocol <- R6::R6Class(
  "RSEncoderProtocol",
  inherit = RSProtocol,
  cloneable = FALSE,
  public = list(

    #' @description
    #' Lists the method names that any class claiming conformance to this
    #' protocol must implement. Specifically, classes conforming to
    #' `RSEncoderProtocol` are required to define a method called `encodeObject`
    #' and `finalize`.
    #'
    #' @return A character vector of required method names.
    #'
    requiredMethods = function() {
      c("encodeObject", "lockInstance")
    }
    # encodeObject = function(instance) {}
    # finalize = function(void) {}
    # #' @keywords optional
    # encodingContainer = function(instance) {}
  )
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
#' - `encodedClasses()`: Returns the dictionary of classID → instance set.
#' - `resolveClassesWithDictionary(class_map)`: Replaces placeholder class definitions
#'   with actual runtime classes.
#'
#' @export
RSDecoderProtocol <- R6::R6Class(
  "RSDecoderProtocol",
  inherit = RSProtocol,
  cloneable = FALSE,
  public = list(

    #' @description
    #' Lists the method names that any class claiming conformance to this
    #' protocol must implement. Specifically, classes conforming to
    #' `RSDecoderProtocol` are required to define a method called
    #' `encodedClasses` and `resolveClassesWithDictionary`.
    #'
    #' @return A character vector of required method names.
    #'
    requiredMethods = function() {
      c("encodedClasses", "resolveClassesWithDictionary")
    }
    # encodedClasses = function(void) {}
    # resolveClassesWithDictionary = function(counter_classes) {}
  )
)
