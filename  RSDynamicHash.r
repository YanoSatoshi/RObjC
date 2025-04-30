RSDynamicHash <- R6::R6Class(
  ### class name ###
  "RSDynamicHash",
  cloneable = FALSE,
  inherit = RSObject,
  public = list(
    hash_operator = NULL, # read_only
    initialize = function(initial_bits = 4) {
      super$initialize()
      private$.bits   <- initial_bits
      private$.size   <- bitwShiftL(1L, private$.bits)
      private$.bucket <- vector("list", private$.size)
      private$.count  <- 0
      self$hash_operator <- RSHashOperator$module()
      lockBinding('hash_operator', self)
      invisible(self)
    },
    count = function() {
      private$.count
    },
    helper = function() {
      private$.helper
    },
    getObjectsByHash = function(hash_code) {
      idx <- private$.hashIndex(hash_code)
      if (!is.null(private$.bucket[[idx]])) {
        items <- private$.bucket[[idx]]
        matches <- vapply(items, function(x) x$hash() == hash_code, logical(1))
        if (any(matches)) {
          return(items[which(matches)])
        }
      }
      return(NULL)
    },
    getObject = function(obj) {
      items <- self$getObjectsByHash(obj$hash())
      if (is.null(items)) return(NULL)
      matches <- vapply(items, function(x) x$isEqual(obj), logical(1))
      if (any(matches)) {
        return(items[[which(matches)[1]]])
      }
      return(NULL)
    },
    containsObject = function(obj) {
      !is.null(self$getObject(obj))
    },
    setObject = function(obj) {
      if (is.null(obj$isMemberOf)) {
        stop("InvalidArgumentError. Cannot store a object without inheriting RSObject")
      }
      hasObject <- self$containsObject(obj)
      if (hasObject) {
        return(invisible(NULL))
      } else {
        idx <- private$.hashIndex(obj$hash())
        items <- private$.bucket[[idx]]
        private$.bucket[[idx]] <- c(items, list(obj))
        private$.count <- private$.count + 1
        if (private$.count > private$.size * 0.7) private$.rehash()
      }
    },
    removeObject = function(obj) {
      obj_hash <- obj$hash()
      items <- self$getObjectsByHash(obj_hash)
      if (is.null(items)) return(invisible(NULL))
      matches <- vapply(items, function(x) x$isEqual(obj), logical(1))
      if (any(matches)) {
        keep_items <- items[!matches]
        idx <- private$.hashIndex(obj_hash)
        private$.bucket[[idx]] <-
          private$.bucket[[idx]] <- if (length(keep_items) > 0) keep_items else list()
        private$.count <- private$.count - sum(matches)
      }
    },
    allObjects = function() {
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
      private$.helper$hashFromObjects(self$allObjects())
    },
    isEqual = function(other) {
      if (!other$isMemberOf(self$class())) return(FALSE)
      private$.helper$areEqualSets(self$allObjects(), other$allObjects())
    }
  ),
  private = list(
    .bits = NULL,
    .size = NULL,
    .bucket = NULL,
    .count  = NULL,
    .helper = NULL,
    .hashIndex = function(raw_hash) {
      as.integer(bitwAnd(raw_hash, (2^private$.bits - 1))) + 1
    },
    .rehash = function() {
      old_bucket <- private$.bucket
      new_bits <- private$.bits + 1
      new_size <- 2^new_bits
      new_bucket <- vector("list", new_size)
      # 旧バケットから全要素を抽出
      all_entries <- unlist(old_bucket, recursive = FALSE)
      # 各エントリに対して新しいインデックスを計算
      hashes <- vapply(all_entries, function(obj) obj$hash(), integer(1))
      new_indices <- as.integer(bitwAnd(hashes, new_size - 1L)) + 1L
      # 空の新バケットを作成
      new_bucket <- vector("list", new_size)
      # バケットにエントリを振り分け（splitでグループ化）
      grouped_entries <- split(all_entries, new_indices)
      # 該当位置にまとめて代入
      new_bucket[as.integer(names(grouped_entries))] <- grouped_entries
      private$.bits <- new_bits
      private$.size <- new_size
      private$.bucket <- new_bucket
      private$.count <- new_count
    },
    .rehashSet = function(obj) {
      idx <- private$.hashIndex(obj$hash())
      if (is.null(private$.bucket[[idx]])) {
        private$.bucket[[idx]] <- list(obj)
      } else {
        private$.bucket[[idx]] <- c(private$.bucket[[idx]], list(obj))
      }
      private$.count <- private$.count + 1
    }
  )
)
