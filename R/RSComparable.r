#' @include api_setup.r
#' @title RSComparable: Protocol for Three-Way Ordering
#'
#' @description
#' `RSComparable` declares `compare(other, na.last)` for RObjC instances that
#' support three-way ordering. A conforming method returns `-1L`, `0L`, or
#' `1L` for a comparable pair and may raise an error for incompatible values.
#' Declaring conformance does not guarantee that every pair of values has an
#' ordering. [RSRaw] inherits this protocol from [RSScalar], but its
#' `compare()` always raises an error because raw values have no sorting order.
#'
#' @export
RSComparable <- RSProtocolClass(
  "RSComparable",
  contract_methods = "compare",
  version = "0.1.0 beta"
)
