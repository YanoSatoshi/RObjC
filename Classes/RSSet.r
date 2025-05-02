RSSet <- R6::R6Class(
  "RSSet",
  inherit = RSMutableSet,
  cloneable = FALSE,
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
  ),
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
    allObjectsAsList = function() {
      private$.flat
    },
    allObjects = function() {
      RSArray$new(objects = self$allObjectsAsList())
    },
    addObject = function(obj) {
      stop(msg_immutable_try_mutate(self$class()))
    },
    removeObject = function(obj) {
      stop(msg_immutable_try_mutate(self$class()))
    },
    removeAllObjects = function() {
      stop(msg_immutable_try_mutate(self$class()))
    },
    copyWithEnv = function(env = parent.frame()) {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSSet$new,
        list(objects = self$allObjectsAsList()),
        envir = env
      )
    },
    mutableCopyWithEnv = function(env = parent.frame()) {
      do.call(
        RSMutableSet$new,
        list(objects = self$allObjectsAsList()),
        envir = env
      )
    }
  )
)
