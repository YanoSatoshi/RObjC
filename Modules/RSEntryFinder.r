#' @title RSEntryFinder: Search Module for Entry Objects
#'
#' @description
#' `RSEntryFinder` is a singleton utility module that provides linear and binary search
#' functionality over lists of RS objects (typically entries in a dictionary or set).
#'
#' It supports searching by applying an optional transformation (`getter`) on each object,
#' and uses `isEqual` and `compare` methods for match detection.
#'
#' @section Main Methods:
#' - `findIndexByLinear(entries, target, getter = identity)`
#' - `findByLinear(entries, target, getter = identity)`
#' - `findIndexByBinary(entries, target, getter = identity)`
#' - `findByBinary(entries, target, getter = identity)`
#'
#' @examples
#' f <- RSEntryFinder$module()
#' idx <- f$findIndexByLinear(entries, key)
#' result <- f$findByBinary(sorted_entries, key)
#'
#' @export
RSEntryFinder <- R6::R6Class(
  "RSEntryFinder",
  inherit = RSModule,
  cloneable = FALSE,
  public = list(
#' @describeIn RSEntryFinder
#' Performs a linear search over a list of entries to find the index of the target object.
#' The `getter` function is applied to each entry before comparison.
#' @param entries A list of RS objects to search.
#' @param target The object to match.
#' @param getter A function applied to each entry before comparison (default: `identity`).
#' @return Integer index if found; otherwise `NA_integer_`.
    findIndexByLinear = function(entries, target, getter = identity) {
      for (i in seq_along(entries)) {
        probe <- getter(entries[[i]])
        if (probe$isEqual(target)) return(i)
      }
      NA_integer_
    },
#' @describeIn RSEntryFinder
#' Performs a linear search and returns the matching object, if found.
#' The `getter` function is applied to each entry before comparison.
#' @param entries A list of RS objects to search.
#' @param target The object to match.
#' @param getter A function applied to each entry before comparison (default: `identity`).
#' @return The matching object, or `NULL` if not found.
    findByLinear = function(entries, target, getter = identity) {
      idx <- self$findIndexByLinear(entries, target, getter)
      if (!is.na(idx)) entries[[idx]] else NULL
    },
#' @describeIn RSEntryFinder
#' Performs a binary search over a sorted list of entries to find the index of the target.
#' Entries must be sorted using a consistent comparator. The `getter` function is applied
#' to each entry before calling `compare()`.
#' @param entries A sorted list of RS objects.
#' @param target The object to locate.
#' @param getter A function applied to each entry before comparison (default: `identity`).
#' @return Integer index if found; otherwise `NA_integer_`.
    findIndexByBinary = function(entries, target, getter = identity) {
      lo <- 1L
      hi <- length(entries)
      while (lo <= hi) {
        mid <- bitwShiftR(lo + hi, 1L)
        probe <- getter(entries[[mid]])
        if (!probe$respondsTo("compare")) break
        cmp <- probe$compare(target)
        if (cmp == 0L && probe$isEqual(target)) return(mid)
        if (cmp < 0L) lo <- mid + 1L else hi <- mid - 1L
      }
      NA_integer_
    },
#' @describeIn RSEntryFinder
#' Performs a binary search and returns the matching object from a sorted list.
#' The `getter` function is applied before comparison.
#' @param entries A sorted list of RS objects.
#' @param target The object to match.
#' @param getter A function applied to each entry before comparison (default: `identity`).
#' @return The matching object, or `NULL` if not found.
    findByBinary = function(entries, target, getter = identity) {
      idx <- self$findIndexByBinary(entries, target, getter)
      if (!is.na(idx)) entries[[idx]] else NULL
    }
  )
)
register_rs_module(RSEntryFinder, RSEntryFinder$new())
