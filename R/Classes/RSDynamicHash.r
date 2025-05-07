#' @title RSDynamicHash: Mutable, Hash-Based Key-Value Container
#'
#' @description
#' `RSDynamicHash` is a hash-table-like mutable data structure for storing uniquely
#' identifiable `RSObject` instances. Keys are derived from each object’s `hash()` method,
#' and equivalence is determined by `isEqual()`. The structure dynamically resizes
#' when the load factor exceeds 75%.
#'
#' Unlike `RSMutableSet` or `RSMutableDictionary`, this class stores objects **as-is**;
#' it does not call `copyWithEnv()` upon insertion. This allows full control over the
#' mutability and lifecycle of stored objects, and is suitable for internal frameworks
#' requiring high-performance mutable object indexing.
#'
#' @section Internal Modules:
#' - `RSHashOperator` — used for hash calculation and equality
#' - `RSEntryFinder` — used for binary/linear search
#' - `RSSorter` — used for reordering buckets during rehashing
#'
#' @section Public Methods:
#' - `setObject()`, `getObject()`, `containsObject()`, `removeObject()`
#' - `removeAllObjects()`, `allObjectsAsList()`, `count()`, `size()`
#' - `hash()`, `isEqual(other)`, `description()`, `debugDescription()`
#'
#' @export
RSDynamicHash <- R6::R6Class(
  ### class name ###
  "RSDynamicHash",
  cloneable = FALSE,
  inherit = RSObject,
  private = list(
    .bits = NULL,
    .size = NULL,
    .bucket = NULL,
    .count  = NULL,
    .hashIndex = function(raw_hash) {
      as.integer(bitwAnd(raw_hash, (bitwShiftL(private$.bits, 1L) - 1))) + 1
    },
    .initializeTable = function() {
      private$.bucket[] <- rep(list(NULL), length(private$.bucket))
      private$.count <- 0
    },
    .rehash = function() {
      old_bucket <- private$.bucket
      new_bits <- private$.bits + 1L
      new_size <- bitwShiftL(1L, new_bits)
      new_bucket <- vector("list", new_size)
      all_entries <- unlist(old_bucket, recursive = FALSE)
      has_compare <-
        length(all_entries) <= 1L || # capable with length 0 list.
        all(
          vapply(all_entries, function(x) x$respondsTo("compare"), logical(1))
        )
      hashes <- vapply(all_entries, function(obj) obj$hash(), integer(1))
      new_indices <- as.integer(bitwAnd(hashes, new_size - 1L)) + 1L
      grouped <- split(all_entries, new_indices)
      sorter <- self$entry_sorter
      for (idx_chr in names(grouped)) {
        i <- as.integer(idx_chr)
        entries <- grouped[[idx_chr]]
        if (has_compare && length(entries) > 1L) {
          sorted_entries <- sorter$sortByCompare(
            entries, function(a, b) a$compare(b)
          )
        } else {
          sorted_entries <- entries
        }
        new_bucket[[i]] <- sorted_entries
      }
      private$.bits   <- new_bits
      private$.size   <- new_size
      private$.bucket <- new_bucket
      private$.count  <- length(all_entries)
      invisible(NULL)
    }
  ),
  public = list(
    hash_operator = NULL, # read_only
    entry_finder  = NULL, # read_only
    entry_sorter  = NULL, # read_only



#' @describeIn RSDynamicHash
#' Constructs a new hash table with an optional list of objects to register.
#' Objects are stored directly without copying. The internal bucket size is `2^initial_bits`.
#'
#' @param initial_bits Integer; log2 of the number of initial buckets.
#' @param objects Optional list of `RSObject`s to register immediately.
#' @return A new `RSDynamicHash` instance.
    initialize = function(initial_bits = 4, objects = list()) {
      super$initialize()
      private$.bits   <- initial_bits
      private$.size   <- bitwShiftL(1L, private$.bits)
      private$.bucket <- vector("list", private$.size)
      private$.count  <- 0
      self$hash_operator <- RSHashOperator$module()
      self$entry_finder  <- RSEntryFinder$module()
      self$entry_sorter  <- RSSorter$module()
      lockBinding("hash_operator", self)
      lockBinding("entry_finder",  self)
      lockBinding("entry_sorter",  self)
      for (obj in objects) self$setObject(obj)
      invisible(self)
    },



#' @describeIn RSDynamicHas
#' Returns the total number of stored objects in the hash table.
#' @return Integer count.
    count = function() {
      private$.count
    },



#' @describeIn RSDynamicHash
#' Retrieves an object from the table that matches the input object
#' by `hash()` and `isEqual()` equivalence.
#'
#' @param obj The `RSObject` to search for.
#' @return The matching object, or `NULL` if not found.
    getObject = function(obj) {
      self$getEntryMatching(obj, identity)
    },



#' @describeIn RSDynamicHash
#' Retrieves the full entry object from the internal hash table whose key matches the input object.
#' Unlike `getObject()`, which returns the stored object itself, this method returns the complete
#' internal entry that contains metadata such as the key and value pair.
#'
#' This method is typically used by dictionary-like structures (e.g., `RSDictionary`)
#' to access both the key and value simultaneously.
#'
#' @param obj The key-like `RSObject` to search for.
#' @return The internal entry object (e.g., `RSKeyValuePair`), or `NULL` if not found.
    getEntryMatching = function(obj, key_getter) {
      idx <- private$.hashIndex(obj$hash())
      bucket <- private$.bucket[[idx]]
      if (is.null(bucket) || length(bucket) == 0) return(NULL)
      index <- NA_integer_
      if (obj$respondsTo("compare")) {
        index <- self$entry_finder$findIndexByBinary(bucket, obj, key_getter)
      } else {
        index <- self$entry_finder$findIndexByLinear(bucket, obj, key_getter)
      }
      if (is.na(index)) return(NULL)
      bucket[[index]]
    },



#' @describeIn RSDynamicHash
#' Checks whether an equivalent object is present in the table.
#' @param obj An `RSObject` to check.
#' @return `TRUE` if present; otherwise `FALSE`.
    containsObject = function(obj) {
      !is.null(self$getEntryMatching(obj, identity))
    },



#' @describeIn RSDynamicHash
#' Registers or replaces an object in the hash table based on its `hash()` and `isEqual()` values.
#' If an equivalent object is already present, it is replaced.
#'
#' @param obj An `RSObject` to store. Must implement `hash()` and `isEqual()`.
#' @return Invisible `self`.
    setObject = function(obj) {
      if (is.null(obj$isMemberOf)) {
        stop("InvalidArgumentError. Object must inherit RSObject.")
      }
      if (private$.count > private$.size * 0.75) private$.rehash()
      idx <- private$.hashIndex(obj$hash())
      bucket <- private$.bucket[[idx]]
      getter <- identity
      index  <- NA_integer_
      if (obj$respondsTo("compare")) {
        index <- self$entry_finder$findIndexByBinary(bucket, obj, getter)
      } else {
        index <- self$entry_finder$findIndexByLinear(bucket, obj, getter)
      }
      if (!is.na(index)) {
        bucket[[index]] <- obj  # replacing
      } else {
        bucket <- append(bucket, list(obj))  # adding
        private$.count <- private$.count + 1L
      }
      private$.bucket[[idx]] <- bucket
      invisible(self)
    },



#' @describeIn RSDynamicHash
#' Removes the specified object from the hash table based on its identity.
#'
#' @param obj An `RSObject` to remove.
#' @return Invisible `NULL`.
    removeObject = function(obj) {
      if (is.null(obj$isMemberOf)) {
        stop("InvalidArgumentError. Object must inherit RSObject.")
      }
      idx <- private$.hashIndex(obj$hash())
      bucket <- private$.bucket[[idx]]
      if (is.null(bucket) || length(bucket) == 0) return(invisible(NULL))
      getter <- identity
      index  <- NA_integer_
      if(obj$respondsTo("compare")) {
        index <- self$entry_finder$findIndexByBinary(bucket, obj, getter)
      } else {
        index <- self$entry_finder$findIndexByLinear(bucket, obj, getter)
      }
      if (!is.na(index)) {
        private$.bucket[[idx]] <- bucket[-index]
        private$.count <- private$.count - 1L
      }
      invisible(NULL)
    },



#' @describeIn RSDynHash
#' Clears the hash table by removing all registered objects.
#'
#' @return Invisible `NULL`.
    removeAllObjects = function() {
      private$.initializeTable()
      invisible(self)
    },



#' @describeIn RSDynamicHash
#' Returns all stored objects in a flat list, preserving bucket contents.
#' @return A list of `RSObject`s.
    allObjectsAsList = function() {
      objs <- list()
      filled_b <- Filter(Negate(is.null), private$.bucket)
      objs <- unlist(filled_b, recursive = FALSE)
      return(objs)
    },



#' @describeIn RSDynamicHash
#' Returns a short string summary of the hash table's state.
#' Format: "<ClassName: N entries, M buckets>".
#'
#' @return Character string.
    description = function() {
      sprintf(
        "<%s: %d entries, %d buckets>",
        self$class()$classname,
        private$.count,
        private$.size
      )
    },



#' @describeIn RSDynamicHash
#' Returns a detailed multi-line string showing bucket-wise occupancy.
#' Useful for debugging internal state.
#' @return Character string.
    debugDescription = function() {
      b_info <- vapply(
        seq_along(private$.bucket),
        function(i) {
          b <- private$.bucket[[i]]
          if (is.null(b)) {
            sprintf("[%d]: (empty)", i - 1L)
          } else {
            sprintf("[%d]: %d items", i - 1L, length(b))
          }
        },
        character(1)
      )
      paste0(
        self$description(), "\n", paste(b_info, collapse = "\n")
      )
    },



#' @describeIn RSDynamicHash
#' Custom print method that calls `description()` and prints to console.
#'
#' @return Invisible `self`.
    print = function(...) {
      cat(self$description(), "\n")
      invisible(self)
    },



#' @describeIn RSDynamicHash
#' Computes an aggregate hash from all stored objects using `RSHashOperator$hashFromObjects()`.
#' @return Integer hash value.
    hash = function() {
      self$hash_operator$hashFromObjects(self$allObjectsAsList())
    },



#' @describeIn RSDynamicHash
#' Compares two hash tables for equality by comparing the contents of their object lists.
#' @param other Another `RSDynamicHash` instance.
#' @return `TRUE` if equivalent; otherwise `FALSE`.
    isEqual = function(other) {
      if (!other$isMemberOf(self$class())) return(FALSE)
      self$hash_operator$areEqualSets(
        self$allObjectsAsList(),
        other$allObjectsAsList()
      )
    },



#' @describeIn RSDynamicHash
#' Returns the number of buckets currently allocated.
#' @return Integer bucket size.
    size = function() {
      return(private$.size)
    }
  )
)
