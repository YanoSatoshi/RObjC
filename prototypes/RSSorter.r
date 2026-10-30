# todo REMOVE ME
# C++ 化以前、Dictionary のハッシュテーブル構築と検索を支えるために用意した
# 並べ替えモジュール。現在の Dictionary は C++ のハッシュテーブルを使い、
# このクラスを参照しない。履歴確認のためソースのみ保存する。

#' @include api_setup.r
#' @title RSSorter: Sorting Module for Proxy and Compare Functions
#'
#' @description
#' `RSSorter` is a singleton utility module that provides efficient sorting algorithms
#' for object lists. It supports proxy-based sorting (via numeric keys)
#' and custom comparator-based quicksort.
#'
#' This module is used for sorting objects in sets, dictionaries, or search modules.
#'
#' @section Main Methods:
#' - `sortByProxyKey(entries, key_fn)` — Sorts using precomputed numeric keys
#' - `sortByCompare(entries, compare_fn)` — Sorts using a custom comparator function
#'
#' @examples
#' \dontrun{
#' s <- RSSorter$instance
#' sorted <- s$sortByProxyKey(entries, function(e) e$hash())
#' }
#'
#' @export
RSSorter <- RSSingletonClass(
  "RSSorter",
  version = "0.1.0 beta",
  super_class = RSObject,
  public = list(

    #' @description
    #' Sorts a list of RS objects by numeric proxy keys extracted using the `key_fn` function.
    #' This method uses `order()` internally and is suitable for large lists with fast key access.
    #'
    #' @param entries A list of RS objects to be sorted.
    #' @param key_fn A function that returns a numeric key for each object (e.g., `hash()`).
    #' @return A list of RS objects sorted by their proxy keys.
    sortByProxyKey = function(entries, key_fn) {
      n <- length(entries)
      if (n <= 1) return(entries)
      keys <- vapply(entries, key_fn, FUN.VALUE = numeric(1L))
      entries[order(keys, method = "radix")]
    },



    #' @description
    #' Sorts a list of RS objects using a custom comparator function.
    #' This method uses a non-recursive quicksort and preserves input order
    #' when the comparator returns zero.
    #'
    #' @param entries A list of RS objects to sort.
    #' @param compare_fn A function of two arguments that returns -1, 0, or 1 for less, equal, or greater.
    #' @return A new list containing the sorted objects.
    sortByCompare = function(entries, compare_fn) {
      n <- length(entries)
      if (n <= 1) return(entries)
      result <- entries
      original_index <- seq_len(n)
      stack <- vector("list", n * 2L)
      top <- 1L
      stack[[top]] <- list(lo = 1L, hi = n)
      while (top > 0L) {
        range <- stack[[top]]
        top <- top - 1L
        lo <- range$lo
        hi <- range$hi
        if (lo >= hi) next  # Skip empty or single-element ranges
        # --- Pivot selection ---
        mid <- bitwShiftR(lo + hi, 1L)  # Select middle index as pivot
        pivot <- result[[mid]]
        pivot_index <- original_index[[mid]]
        # --- Move pivot to the end ---
        result[[mid]] <- result[[hi]]
        result[[hi]] <- pivot
        original_index[[mid]] <- original_index[[hi]]
        original_index[[hi]] <- pivot_index
        # --- Partition step ---
        i <- lo
        for (j in lo:(hi - 1L)) {
          comparison <- compare_fn(result[[j]], pivot)
          if (comparison < 0L ||
              (comparison == 0L && original_index[[j]] < pivot_index)) {
            # Swap result[[i]] and result[[j]]
            result[c(i, j)] <- result[c(j, i)]
            original_index[c(i, j)] <- original_index[c(j, i)]
            i <- i + 1L
          }
        }
        # --- Place pivot in final position ---
        result[c(i, hi)] <- result[c(hi, i)]
        original_index[c(i, hi)] <- original_index[c(hi, i)]
        # --- Push subranges to stack for later processing ---
        # Left side: [lo, i - 1]
        if (i - 1L > lo) {
          top <- top + 1L
          stack[[top]] <- list(lo = lo, hi = i - 1L)
        }
        # Right side: [i + 1, hi]
        if (i + 1L < hi) {
          top <- top + 1L
          stack[[top]] <- list(lo = i + 1L, hi = hi)
        }
      }
      result
    }
  )
)
