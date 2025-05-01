RSHashOperator <- R6::R6Class(
  "RSHashOperator",
  inherit = RSModule,
  cloneable = FALSE,
  public = list(
    initialize = function() {
      super$initialize()
      invisible(self)
    },
    hashesOfEachObject = function(objects) {
      if (length(objects) == 0) return(0L)
      vapply(objects, function(obj) obj$hash(), integer(1))
    },
    hashFromObjects = function(objects) {
      hashes <- vapply(objects, function(obj) obj$hash(), integer(1))
      Reduce(bitwXor, hashes)
    },
    areEqualSets = function(obj1, obj2) {
      if (length(obj1) != length(obj2)) return(FALSE)
      obj1_hashes <- sort(self$hashesOfEachObject(obj1))
      obj2_hashes <- sort(self$hashesOfEachObject(obj2))
      if (!identical(obj1_hashes, obj2_hashes)) return(FALSE)
      differs_by_method <- any(vapply(
        obj1,
        function(o1) {
          o1_hash <- o1$hash()
          !any(vapply(obj2, function(o2) {
            o1_hash == o2$hash() && isTRUE(o1$isEqual(o2))
          }, logical(1)))
        },
        logical(1)
      ))
      return(!differs_by_method)
    },
    objectsNotIn = function(obj1, obj2) {
      obj2_hashes <- self$hashesOfEachObject(obj2)
      Filter(
        function(o1) {
          o1_hash <- o1$hash()
          # Step 1: early reject via hash mismatch
          if (!(o1_hash %in% obj2_hashes)) return(TRUE)
          # Step 2: confirm absence by strict isEqual check
          !any(vapply(
            obj2, function(o2) o1$isEqual(o2), logical(1)
          ))
        },
        obj1
      )
    },
    objectsAlsoIn = function(obj1, obj2) {
      obj2_hashes <- self$hashesOfEachObject(obj2)
      Filter(
        function(o1) {
          o1_hash <- o1$hash()
          if (!(o1_hash %in% obj2_hashes)) return(FALSE)
          any(vapply(
            obj2,
            function(o2) {
              isTRUE(o1_hash == o2$hash() && o1$isEqual(o2))
            },
            logical(1)
          ))
        },
        obj1
      )
    },
    unionOfSets = function(obj1, obj2) {
      c(obj2, self$objectsNotIn(obj1, obj2))
    },
    intersectionOfSets = function(obj1, obj2) {
      self$objectsAlsoIn(obj1, obj2)
    },
    differenceOfSets = function(obj1, obj2) {
      self$objectsNotIn(obj1, obj2)
    },
    hashForScalar = function(val) {
      str_x <- as.character(val)
      code_points <- utf8ToInt(str_x)
      if (length(code_points) == 0) return(0L)
      # ベクトル化: 左シフト量をあらかじめ計算して、ビット演算をベクトルでまとめる
      shifts  <- bitwShiftR((seq_along(code_points) - 1L), 5L)  # シフト量をサイクルさせる
      shifted <- bitwShiftL(code_points, shifts)
      h <- Reduce(bitwXor, shifted, init = 0L)
      abs(as.integer(h))
    }
  )
);
RSHashOperator$new();
# Module instance and accessor
RSHashOperator$module <- function() {
  RSHashOperator$.instance
}
