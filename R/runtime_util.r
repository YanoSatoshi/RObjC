#' @title Get an R Object's Address
#'
#' @description
#' Returns the memory address of any R object as a hexadecimal string. R6
#' instances and class generators are environments, so aliases of the same
#' instance or generator have the same address within the current R session.
#'
#' Value objects such as atomic vectors, lists, data frames, and tibbles can be
#' copied when modified (copy-on-write). After a copy, the modified binding may
#' have a different address. This includes modifications to attributes. The
#' address is therefore not a stable identity for value objects. Addresses are
#' not persistent across R sessions, and an address may be reused after its
#' object is released.
#'
#' @param some_obj Any R object.
#'
#' @return A character string containing the object's address, such as
#'   `"0x55abcde12345"`.
#'
#' @examples
#' obj <- RSObject$new()
#' robjc_get_pointer(obj)
#' robjc_get_pointer(RSObject)
#' robjc_get_pointer(list(a = 1L))
#' robjc_get_pointer(1:3)
#'
#' @seealso [RSObject]
#'
#' @export
robjc_get_pointer <- function(some_obj) robjc_object_address(some_obj)





#' @title Normalize a Hash Code
#' @description Replace a missing hash code with `0L`. Call this on the
#'   final result of a custom `hash()` method so it never returns R's
#'   `NA_integer_` bit pattern. Equal objects must still produce equal hashes.
#' @param hash_code One integer hash code, possibly `NA_integer_`.
#' @return `0L` for a missing hash code; otherwise `hash_code` unchanged.
#' @examples
#' robjc_normalize_hash(NA_integer_)
#' robjc_normalize_hash(42L)
#' @seealso [RSObject]
#' @export
robjc_normalize_hash <- function(hash_code) if (is.na(hash_code)) 0L else hash_code
