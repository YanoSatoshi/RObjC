RSSorter <- R6::R6Class(
  "RSSorter",
  inherit = RSModule,
  cloneable = FALSE,
  public = list(
    # proxy key に基づく高速ソート（order 使用）
    sortByProxyKey = function(entries, key_fn) {
      n <- length(entries)
      if (n <= 1) return(entries)
      keys <- vapply(entries, key_fn, FUN.VALUE = numeric(1L))
      entries[order(keys, method = "radix")]
    },
    # 高速クイックソート（非再帰・任意比較関数）
    sortByCompare = function(entries, compare_fn) {
      n <- length(entries)
      if (n <= 1) return(entries)
      result <- entries
      stack <- vector("list", n * 2L)
      top <- 1L
      stack[[top]] <- list(lo = 1L, hi = n)
      while (top > 0L) {
        range <- stack[[top]]
        top <- top - 1L
        lo <- range$lo
        hi <- range$hi
        if (lo >= hi) next
        # 中央 pivot 選択
        mid <- bitwShiftR(lo + hi, 1L)
        pivot <- result[[mid]]
        # pivot を末尾に移動（退避）
        result[[mid]] <- result[[hi]]
        result[[hi]] <- pivot
        i <- lo
        for (j in lo:(hi - 1L)) {
          if (compare_fn(result[[j]], pivot) < 0L) {
            result[c(i, j)] <- result[c(j, i)]
            i <- i + 1L
          }
        }
        # pivot を正しい位置に戻す
        result[c(i, hi)] <- result[c(hi, i)]
        # ソート範囲をスタックに追加
        if (i - 1L > lo) {
          top <- top + 1L
          stack[[top]] <- list(lo = lo, hi = i - 1L)
        }
        if (i + 1L < hi) {
          top <- top + 1L
          stack[[top]] <- list(lo = i + 1L, hi = hi)
        }
      }
      result
    }
  )
)
register_rs_module(RSSorter, RSSorter$new())