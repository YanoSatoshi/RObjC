#' Class and Protocol Index
#'
#' @description
#' Built-in RObjC class and protocol generators, grouped by use. See the
#' [factory API index][factory_api_index] to declare your own classes.
#'
#' @section Base classes and modules:
#' - [RSObject] is the base object class.
#' - [RSScalar] is the base scalar wrapper class.
#' - [RSModule] is the base class for shared utility modules.
#'
#' @section Scalar values:
#' - [RSNull] represents `NULL`.
#' - [RSBoolean] and [RSNABoolean] represent logical values.
#' - [RSNumber] and [RSNANumber] represent numeric values.
#' - [RSComplex] and [RSNAComplex] represent complex values.
#' - [RSRaw] represents raw bytes.
#' - [RSString] and [RSNAString] represent character values.
#' - [RSFactor] and [RSNAFactor] represent factors.
#' - [RSDate] and [RSNADate] represent dates.
#' - [RSDateTime] and [RSNADateTime] represent date-times.
#'
#' @section Collections:
#' - [RSArray] and [RSMutableArray] are ordered containers.
#' - [RSVector] and [RSMutableVector] wrap atomic vectors.
#' - [RSSet] and [RSMutableSet] store unique objects.
#' - [RSDictionary] and [RSMutableDictionary] store key-value pairs.
#' - [RSDataFrame] and [RSMutableDataFrame] wrap data frames.
#'
#' @section Protocols:
#' - [RSProtocol] is the root protocol generator.
#' - [RSComparable] declares three-way ordering for comparable instances.
#' - [RSCopying] and [RSMutableCopying] define copying contracts.
#' - [RSCoding] marks classes supporting keyed coding;
#'   [RSEncoderProtocol] and [RSDecoderProtocol] declare its two methods.
#'
#' @section Utility classes:
#' - [RSHashOperator] provides shared hashing and set operations.
#'
#' @seealso [robjc-package], [factory_api_index], [runtime_functions]
#' @name class_index
#' @aliases robjc_classes
NULL
