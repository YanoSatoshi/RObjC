RSSet <- R6::R6Class(
  "RSSet",
  inherit = RSMutableSet,
  cloneable = FALSE,
  public = list(
    # 初期化時に、引数 objects (リスト) に含まれる要素を追加し、freeze する
    initialize = function(objects = list()) {
      super$initialize(objects)
      # 追加操作完了後、freeze して内部構造を不変化する
      private$.freeze()
      invisible(self)
    },
    containsObject = function(obj) {
      # .flat に対してベクトル化して調べる
      if (length(private$.flat) == 0) return(FALSE)
      obj_hash <- obj$hash()
      flags <- vapply(
        private$.flat,
        function(x) {x$hash() == obj_hash && x$isEqual(obj)},
        logical(1)
      )
      any(flags)
    },
    count = function() {
      length(private$.flat)
    },
    allObjects = function() {
      private$.flat
    },
    clear = function() {
      stop("RSSet is immutable. Use a mutable set for modification.")
    }
  ),
  private = list(
    # フラット化した要素リスト。immutable なので、このベクトルを基にすべての操作を行う
    .flat = NULL,
    # freeze: RSDynamicHash のバケット構造を flatten する
    .freeze = function() {
      # 既存の hash table (private$.bucket) をすべて flatten
      if (is.null(private$.bucket)) {
        private$.flat <- list()
      } else {
        flat <- unlist(private$.bucket, recursive = FALSE)
        private$.flat <- flat
      }
      # フラット化後、内部のバケット構造は不要と判断し破棄する
      private$.bucket <- NULL
    }
  )
)
