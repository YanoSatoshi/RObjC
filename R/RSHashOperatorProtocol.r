#' @title RSHashOperatorProtocol Protocol
#'
#' @description
#' The \code{RSHashOperatorProtocol} protocol defines the required interface for
#' hash operator modules used by RS scalar and collection classes. It ensures
#' consistent hash computation and set-based logic operations.
#'
#' A conforming module must implement the following methods:
#'
#' @section Required Methods:
#' \describe{
#'
#'   \item{\code{hashForScalar(val)}}{
#'     Computes a deterministic hash value from a scalar input such as a string, number, or logical.
#'     The output must support equality comparison and be stable across sessions.
#'     The specific format (e.g., integer, string, raw vector) is left to the implementation.
#'   }
#'
#'   \item{\code{computeHashCodeWithString(string)}}{
#'     Generates a hash value from a UTF-8 string input.
#'     This method is typically used to create hashes from textual keys or labels.
#'     The return value may be of any hashable type (e.g., numeric, uint64) as long as equality semantics apply.
#'   }
#'
#'   \item{\code{hashesOfEachObject(objects)}}{
#'     Given a list of objects that implement a \code{hash()} method, this method returns
#'     their respective hash values in a vector or list.
#'     The return type must support element-wise equality comparison and must align with how object identity is determined.
#'   }
#'
#'   \item{\code{hashFromObjects(objects)}}{
#'     Computes an aggregate hash value from a set of objects.
#'     This is used to represent the collective identity of a group of elements.
#'     The method must return a deterministic and comparable value (e.g., using reduction, XOR, concatenation, etc.).
#'     The internal strategy is implementation-dependent.
#'   }
#'
#'   \item{\code{areEqualSets(obj1, obj2)}}{
#'     Returns \code{TRUE} if two object lists represent the same unordered set.
#'     Internally, the function must compare both hash values and the results of
#'     each element’s \code{isEqual()} method.
#'     Used for equality checks between containers like \code{Set} or \code{Dictionary}.
#'   }
#'
#'   \item{\code{objectsAlsoIn(obj1, obj2)}}{
#'     Returns the list of objects in \code{obj1} that are also present in \code{obj2}.
#'     Matching requires both hash equality and \code{isEqual()} returning \code{TRUE}.
#'     This is used for set intersection.
#'   }
#'
#'   \item{\code{objectsNotIn(obj1, obj2)}}{
#'     Returns a list of objects from \code{obj1} that are not in \code{obj2}.
#'     Objects are excluded if both hash and \code{isEqual()} match against any element in \code{obj2}.
#'     Used for set difference and filtering.
#'   }
#'
#'   \item{\code{unionOfSets(obj1, obj2)}}{
#'     Returns the union of two object sets.
#'     This includes all elements of \code{obj2} and any unique elements from \code{obj1}.
#'     Duplicates are avoided based on hash and equality semantics.
#'   }
#'
#'   \item{\code{intersectionOfSets(obj1, obj2)}}{
#'     Returns objects common to both \code{obj1} and \code{obj2}.
#'     Equivalent to \code{objectsAlsoIn(obj1, obj2)}; provided for semantic clarity.
#'   }
#'
#'   \item{\code{differenceOfSets(obj1, obj2)}}{
#'     Returns objects in \code{obj1} that are not found in \code{obj2}.
#'     Equivalent to \code{objectsNotIn(obj1, obj2)}; included for expressiveness.
#'   }
#'
#' }
#'
#' @note
#' All methods must be deterministic and consistent with the semantics of \code{hash()} and \code{isEqual()} on RS objects.
#' The hash value format is not fixed but must support equality comparison.
#' Modules implementing this protocol are used by scalar, array, set, and
#' dictionary classes.
#'
#' @seealso \code{\link{RSHashOperator}} for a reference implementation.
RSHashOperatorProtocol <- R6::R6Class(
  "RSHashOperatorProtocol",
  inherit = RSProtocol,
  cloneable = FALSE,
  public = list(
    #' @description Required method for RSHashOperatin conformance.
    #' @return required method name
    requiredMethods = function() c(
      "hashForScalar",
      "computeHashCodeWithString",
      "hashesOfEachObject",
      "hashFromObjects",
      "areEqualSets",
      "objectsAlsoIn",
      "objectsNotIn",
      "unionOfSets",
      "intersectionOfSets",
      "differenceOfSets"
    )
  )
)
