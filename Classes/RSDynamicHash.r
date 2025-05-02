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
      # flatten entries
      all_entries <- unlist(old_bucket, recursive = FALSE)
      # compare() に対応しているか事前チェック（n==0 でもOKにする）
      has_compare <-
        length(all_entries) <= 1L ||
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
    count = function() {
      private$.count
    },
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
    getObject = function(obj) {
      self$getEntryMatching(obj, identity)
    },
    containsObject = function(obj) {
      !is.null(self$getEntryMatching(obj, identity))
    },
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
    removeAllObjects = function() {
      private$.initializeTable()
      invisible(self)
    },
    allObjectsAsList = function() {
      objs <- list()
      filled_b <- Filter(Negate(is.null), private$.bucket)
      objs <- unlist(filled_b, recursive = FALSE)
      return(objs)
    },
    description = function() {
      sprintf(
        "<%s: %d entries, %d buckets>",
        self$class()$classname,
        private$.count,
        private$.size
      )
    },
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
    print = function(...) {
      cat(self$description(), "\n")
      invisible(self)
    },
    hash = function() {
      self$hash_operator$hashFromObjects(self$allObjectsAsList())
    },
    isEqual = function(other) {
      if (!other$isMemberOf(self$class())) return(FALSE)
      self$hash_operator$areEqualSets(
        self$allObjectsAsList(),
        other$allObjectsAsList()
      )
    },
    size = function() {
      return(private$.size)
    }
  )
)
