#' @include api_setup.r
#' @title RSCoder: Base Class for Keyed Coding
#' @description
#' Base interface shared by keyed archivers and unarchivers. A class records its
#' logical state through `encodeObjectForKey()` and reads it through
#' `decodeObjectForKey()`. The concrete coder owns reference tracking and the
#' archive format.
#' @export
RSCoder <- RSObjectClass(
  "RSCoder",
  version = "0.1.0 beta",
  super_class = RSObject,
  public = list(
    #' @description Records a logical field in an archive.
    #' @param object An RObjC object, class, protocol, base R value, or list.
    #' @param key One nonempty character string.
    encodeObjectForKey = function(object, key) {
      stop("encodeObjectForKey() requires an RSKeyedArchiver.", call. = FALSE)
    },

    #' @description Reads a logical field from an archive.
    #' @param key One nonempty character string.
    #' @return The decoded value.
    decodeObjectForKey = function(key) {
      stop("decodeObjectForKey() requires an RSKeyedUnarchiver.", call. = FALSE)
    }
  )
)

.robjc_coder_check_key <- function(key) {
  if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key))
    stop("`key` must be one nonempty character string.", call. = FALSE)
  invisible(key)
}
