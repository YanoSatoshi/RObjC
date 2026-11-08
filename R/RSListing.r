#' @include api_setup.r
#' @title RSListing: Protocol for List-Based Iteration
#'
#' @description
#' `RSListing` declares `asList()` for objects that expose their iteration
#' elements as an R list. Use `for (item in collection$asList())` to iterate.
#' Arrays and sets list their members; dictionaries list their keys; vectors
#' list their individual native values. Set and dictionary order is unspecified.
#'
#' @section Required Method:
#' - `asList()` — Returns an R list of iteration elements.
#'
#' @export
RSListing <- RSProtocolClass(
  "RSListing",
  contract_methods = c("asList"),
  version = "0.2.0"
)
