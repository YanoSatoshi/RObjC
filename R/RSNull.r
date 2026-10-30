#' @include api_setup.r
#' @include RSScalar.r
#' @title RSNull: Singleton Wrapper Representing R's NULL
#'
#' @description
#' `RSNull` is a singleton class derived from `RSScalar` that represents the `NULL` value in R.
#' It provides a uniform object-based null placeholder, similar to `NSNull` in Objective-C,
#' and is designed to be used where scalar-wrapping behavior is required even for `NULL`.
#'
#' [RSSingletonClass()] creates the shared instance when `RSNull` is declared.
#' Access it through `RSNull$null` (an alias of `RSNull$instance`).
#'
#' @section Methods:
#' - `value()` – Always returns `NULL`
#' - `hash()` – Always returns `0L`
#' - `isEqual(other)` – Returns TRUE only for the same instance
#' - `compare(other)` – Returns `0L` for the same instance; other objects are incomparable
#' - `nativeCompare(other)` – Uses the same singleton comparison rule
#' - `description()` – Returns a printable description of the null singleton
#' 
#' @section Usage:
#' `RSNull$null`, `RSNull$instance`, and `RSNull$new()` return the same shared
#' instance. Calling `$new()` does not run `initialize()` again.
#'
#' @note
#' The `RSNull$instance` and `RSNull$null` bindings are locked after creation.
#'
#' @examples
#' x <- RSNull$null
#' x$value()      # returns NULL
#' x$isEqual(RSNull$null)  # TRUE
#'
#' @export
RSNull <- RSSingletonClass(
  "RSNull",
  super_class = RSScalar,
  version     = "0.1.0 beta",
  public      = list(
    #' @description
    #' Initializes an `RSNull` instance. The factory invokes it to create the
    #' shared instance; use `RSNull$null` to retrieve that instance.
    #'
    #' It delegates scalar initialization, then stores `NULL` as the value.
    #'
    #' @return The initialized `RSNull` instance (invisible).
    initialize = function() {
      if (!is.null(self$class()$instance)) {
        class_name <- self$class()$classname
        stop(sprintf("Use `%s$null`. Do not call `%s$new()`.",
                     class_name, class_name), call. = FALSE)
      }
      super$initialize(0L)
      private$.val <- NULL
      invisible(self)
    },

    #' @description Encodes the null singleton without scalar value fields.
    #' @param coder An [RSKeyedArchiver] recording keyed values.
    #' @return `self`, invisibly.
    encodeWithCoder = function(coder) invisible(self),

    #' @description Returns the already shared null singleton.
    #' @param coder An [RSKeyedUnarchiver] reading keyed values.
    #' @return The shared instance, invisibly.
    initWithCoder = function(coder) invisible(self),



    #' @description Compare the singleton with itself.
    #' @param other An object to compare with.
    #' @param na.last Whether missing values sort last. Coerced to one
    #'   nonmissing logical value, for consistency with `RSScalar$compare()`.
    #' @return `0L` for the shared `RSNull` instance. Other objects are
    #'   incomparable and cause an error.
    compare = function(other, na.last = TRUE) {
      na.last <- tryCatch(as.logical(na.last), error = function(e) NA)
      if (!is.logical(na.last) || length(na.last) != 1L || is.na(na.last))
        stop("`na.last` must be TRUE or FALSE.", call. = FALSE)
      if (!identical(self, other))
        stop("Null objects are not comparable.", call. = FALSE)
      0L
    },



    #' @description Compare the null singleton using the same rule as `compare()`.
    #' @param other An object to compare with.
    #' @param na.last Whether missing values sort last.
    #' @return `0L` for the shared `RSNull` instance; otherwise an error.
    nativeCompare = function(other, na.last = TRUE) {
      self$compare(other, na.last = na.last)
    },



    #' @description Return a one-line description of the null singleton.
    #' @return A character scalar containing the class name, pointer, and `NULL`.
    description = function() {
      sprintf("<%s %s> NULL", self$class()$classname, robjc_get_pointer(self))
    },



    #' @description
    #' Determines whether another object is identical to self: `RSNull` singleton.
    #'
    #' @param other Another object to compare against.
    #' @return `TRUE` if `other` is also an `RSNull` instance; otherwise `FALSE`.
    isEqual = function(other) identical(self, other),



    #' @description
    #' Returns a fixed integer hash value (`0L`) for all `RSNull` instances.
    #' Since `RSNull` is a singleton and always represents the same conceptual null value,
    #' a constant hash ensures consistent behavior in hashed collections. The
    #' final `0L` also passes through [robjc_normalize_hash()].
    #'
    #' @return Integer scalar `0L`.
    hash    = function() robjc_normalize_hash(0L),



    #' @description
    #' Returns the internal null value (`NULL`) wrapped by the `RSNull` instance.
    #'
    #' @return `NULL`.
    value = function() NULL
  )
)
RSNull$null <- RSNull$instance
RSNull$is_na_class <- NA
# .onLoad() restores these locks on the package-defined generator after lazy loading.
lockBinding("null", RSNull)
lockBinding("is_na_class", RSNull)
