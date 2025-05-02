RSEntryFinder <- R6::R6Class(
  "RSEntryFinder",
  inherit = RSModule,
  cloneable = FALSE,
  public = list(
    findIndexByLinear = function(entries, target, getter = identity) {
      for (i in seq_along(entries)) {
        probe <- getter(entries[[i]])
        if (probe$isEqual(target)) return(i)
      }
      NA_integer_
    },
    findByLinear = function(entries, target, getter = identity) {
      idx <- self$findIndexByLinear(entries, target, getter)
      if (!is.na(idx)) entries[[idx]] else NULL
    },
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
    findByBinary = function(entries, target, getter = identity) {
      idx <- self$findIndexByBinary(entries, target, getter)
      if (!is.na(idx)) entries[[idx]] else NULL
    }
  )
)
register_rs_module(RSEntryFinder, RSEntryFinder$new())
