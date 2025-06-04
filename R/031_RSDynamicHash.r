#' @title RSDynamicHash: Mutable, Hash-Based Key-Value Container
#'
#' @description
#' `RSDynamicHash` is a hash-table-like mutable data structure for storing uniquely
#' identifiable `RSObject` instances. Keys are derived from each object’s `hash()` method,
#' and equivalence is determined by `isEqual()`. The structure dynamically resizes
#' when the load factor exceeds 75%.
#'
#' Unlike `RSMutableSet` or `RSMutableDictionary`, this class stores objects **as-is**;
#' it does not call `copy()` upon insertion. This allows full control over the
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
  inherit = RSObject,
  cloneable = FALSE,
  private = list(
    .key_getter = NULL,
    .bits = NULL,
    .size = NULL,
    .bucket = NULL,
    .count  = NULL,
    .hashIndex = function(raw_hash) {
      filter <- bitwShiftL(1L, private$.bits) - 1L # (1L << bits) - 1L
      as.integer(bitwAnd(raw_hash, filter) + 1L)   # `+1L` to avoid hash 0
    },
    .getBucket = function(obj) {
      idx <- private$.hashIndex(private$.key_getter(obj)$hash())
      return(list(index = idx, bucket = private$.bucket[[idx]]))
    },
    .getEntryIndex  = function(obj, bucket) {
      key <- private$.key_getter(obj)
      idx <- if (key$respondsTo("compare")) {
        self$entry_finder$findIndexByBinary(bucket, obj, private$.key_getter)
      } else {
        self$entry_finder$findIndexByLinear(bucket, obj, private$.key_getter)
      }
      return(idx)
    },
    .rehash = function() {
      old_bucket <- private$.bucket
      new_bits   <- private$.bits + 1L
      new_size   <- bitwShiftL(1L, new_bits)
      new_bucket <- vector("list", new_size)
      all_entries <- unlist(old_bucket, recursive = FALSE)
      has_compare <- all(
        vapply(all_entries, function(x) x$respondsTo("compare"), logical(1))
      )
      hashes <- vapply(
        all_entries, function(o) private$.key_getter(o)$hash(), integer(1)
      )
      bit_mask    <- bitwShiftL(1L, new_bits) - 1L
      new_indices <- as.integer(bitwAnd(hashes, bit_mask) + 1L)
      grouped <- split(all_entries, new_indices)
      sorter  <- self$entry_sorter
      for (idx_chr in names(grouped)) {
        i <- as.integer(idx_chr)
        entries <- grouped[[idx_chr]]
        if (has_compare && length(entries) > 1L) {
          new_bucket[[i]] <- sorter$sortByCompare(entries, function(a, b) a$compare(b))
        } else {
          new_bucket[[i]] <- entries
        }
      }
      private$.bits   <- new_bits
      private$.size   <- new_size
      private$.bucket <- new_bucket
      private$.count  <- length(all_entries)
      invisible(NULL)
    }
  ),
  public = list(

    #' @field hash_operator An object (module) responsible for computing hash values of stored keys.
    #' Must conform to the hash operator protocol. Read-only.
    hash_operator = NULL, # read_only

    #' @field entry_finder A module responsible for locating entries by key.
    #' Used internally for key lookup and duplicate detection. Read-only.
    entry_finder  = NULL, # read_only

    #' @field entry_sorter A module responsible for ordering entries by key (if applicable).
    #' Typically used for sorting keys in a consistent order. Read-only.
    entry_sorter  = NULL, # read_only



    #' @description
    #' Constructs a new hash table with an optional list of objects to register.
    #' Objects are stored directly without copying. The internal bucket size is `2^initial_bits`.
    #'
    #' @param initial_bits Integer; log2 of the number of initial buckets.
    #' @param objects Optional list of `RSObject`s to register immediately.
    #' @param key_getter Optional function to identify getting hash/isEqual target.
    #' @return A new `RSDynamicHash` instance.
    initialize = function(initial_bits = 4,
                          objects = list(),
                          key_getter = identity) {
      super$initialize()
      private$.key_getter <- key_getter
      private$.bits   <- initial_bits
      private$.size   <- bitwShiftL(1L, private$.bits) # 1L << bits
      private$.bucket <- vector("list", private$.size)  # malloc(sizeof(void*), size)
      private$.count  <- 0
      self$hash_operator <- RSHashOperator$module()
      self$entry_finder  <- RSEntryFinder$module()
      self$entry_sorter  <- RSSorter$module()
      lockBinding("hash_operator", self)
      lockBinding("entry_finder",  self)
      for (obj in objects) self$setObject(obj)
      invisible(self)
    },


    #' @description
    #' Returns the total number of stored objects in the hash table.
    #' @return Integer count.
    count = function() private$.count,



    #' @description
    #' Retrieves an object from the table that matches the input object
    #' by `hash()` and `isEqual()` equivalence.
    #'
    #' @param obj The `RSObject` to search for.
    #' @return The matching object, or `NULL` if not found.
    getObject = function(obj) {
      index_bucket <- private$.getBucket(obj)
      if (is.null(index_bucket$bucket) || length(index_bucket$bucket) == 0) {
        return(NULL) # rough screen
      }
      index <- private$.getEntryIndex(obj, index_bucket$bucket)
      if (is.na(index)) return(NULL)
      return(index_bucket$bucket[[index]])
    },






    #' @description
    #' Checks whether an equivalent object is present in the table.
    #' @param obj An `RSObject` to check.
    #' @return `TRUE` if present; otherwise `FALSE`.
    containsObject = function(obj) !is.null(self$getObject(obj)),



    #' @description
    #' Registers or replaces an object in the hash table based on its `hash()` and `isEqual()` values.
    #' If an equivalent object is already present, it is replaced.
    #'
    #' @param obj An `RSObject` to store. Must implement `hash()` and `isEqual()`.
    #' @return Invisible `self`.
    setObject = function(obj) {
      stopifnot("InvalidArgumentError. `obj` must inherit RSObject." = is_robjc_instance(obj))
      if (private$.count > private$.size * 0.75) private$.rehash()
      index_bucket <- private$.getBucket(obj)
      idx    <- index_bucket$index
      bucket <- index_bucket$bucket
      index  <- private$.getEntryIndex(obj, bucket)
      if (!is.na(index)) {
        bucket[[index]] <- obj  # replacing
      } else {
        bucket <- append(bucket, list(obj))  # adding
        private$.count <- private$.count + 1L
      }
      private$.bucket[[idx]] <- bucket
      invisible(self)
    },



    #' @description
    #' Removes the specified object from the hash table based on its identity.
    #'
    #' @param obj An `RSObject` to remove.
    #' @return Invisible `NULL`.
    removeObject = function(obj) {
      stopifnot("InvalidArgumentError. `obj` must inherit RSObject." = is_robjc_instance(obj))
      index_bucket <- private$.getBucket(obj)
      if (is.null(index_bucket$bucket) || length(index_bucket$bucket) == 0) {
        return(invisible(NULL))
      }
      index <- private$.getEntryIndex(obj, index_bucket$bucket)
      if (!is.na(index)) {
        private$.bucket[[index_bucket$index]] <- index_bucket$bucket[-index]
        private$.count <- private$.count - 1L
      }
      invisible(NULL)
    },



    #' @description
    #' Clears the hash table by removing all registered objects.
    #'
    #' @return Invisible `NULL`.
    removeAllObjects = function() {
      private$.bucket[] <- vector("list", length(private$.bucket))
      private$.count <- 0
    },



    #' @description
    #' Returns all stored objects in a flat list, preserving bucket contents.
    #' @return A list of `RSObject`s.
    allObjectsAsList = function() {
      unlist(Filter(Negate(is.null), private$.bucket), recursive = FALSE)
    },



    #' @description
    #' Returns a short string summary of the hash table's state.
    #' Format: "<ClassName: N entries, M buckets>".
    #'
    #' @return Character string.
    description = function() {
      paste(
        super$description(),
        sprintf("%d entries, %d buckets", private$.count, private$.size)
      )
    },




    #' @description
    #' Returns a detailed multi-line string showing bucket-wise occupancy.
    #' Useful for debugging internal state.
    #' 
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



    #' @description
    #' Custom print method that calls `description()` and prints to console.
    #'
    #' @param ... ignored
    #' @return Invisible `self`.
    print = function(...) {
      cat(self$description(), "\n")
      invisible(self)
    },



    #' @description
    #' Computes an aggregate hash from all stored objects using `RSHashOperator$hashFromObjects()`.
    #' 
    #' @return Integer hash value.
    hash = function() {
      self$hash_operator$hashFromObjects(self$allObjectsAsList())
    },



    #' @description
    #' Compares two hash tables for equality by comparing the contents of their object lists.
    #' 
    #' @param other Another `RSDynamicHash` instance.
    #' @return `TRUE` if equivalent; otherwise `FALSE`.
    isEqual = function(other) {
      if (!other$isMemberOf(self$class())) return(FALSE)
      self$hash_operator$areEqualSets(
        self$allObjectsAsList(), other$allObjectsAsList()
      )
    },



    #' @description
    #' Returns the number of buckets currently allocated.
    #' 
    #' @return Integer bucket size.
    size = function() return(private$.size),



    #' @description
    #'
    #' @return List of content objects
    objectForEncoding = function() self$allObjectsAsList()
  )
)
