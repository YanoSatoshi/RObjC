#' @include api_setup.r
#' @title RSHashOperatorProtocol: Hash Computation Contract
#'
#' @description
#' `RSHashOperatorProtocol` declares the four hash computation methods
#' implemented by [RSHashOperator]. Collection classes use the shared
#' `RSHashOperator$instance`; they do not accept per-instance operators.
#'
#' @section Declared Methods:
#' - `hashForScalar(val)` returns one integer hash for a scalar R value.
#' - `computeHashCodeWithString(string)` returns one integer hash for a string.
#' - `hashesOfObjects(objects)` returns an integer hash for each object.
#' - `combinedHashFromObjects(objects)` returns one aggregate integer hash.
#'
#' Hashes must agree with the equality semantics of their inputs. Aggregate
#' hashes should pass through [robjc_normalize_hash()] to avoid `NA_integer_`.
#'
#' @seealso [RSHashOperator]
RSHashOperatorProtocol <- RSProtocolClass(
  "RSHashOperatorProtocol",
  contract_methods = c(
    "hashForScalar",
    "computeHashCodeWithString",
    "hashesOfObjects",
    "combinedHashFromObjects"
  ),
  version = "0.1.0 beta"
)
