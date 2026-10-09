## RSArray
RSArray_empty <- function(void) RSArray$new()
RSArray_withObject <- function(RSObject object) RSArray$new(list(object))
RSArray_withObjects <- function(list[RSObject] array) RSArray$new(objects)
RSArray_withArray <- function(RSMutableArray array) array$copy()

## RSMutableArray
RSMutableArray_empty <- function(void) RSMutableArray$new()
RSMutableArray_withObject <- function(RSObject object) RSMutableArray$new(list(object))
RSMutableArray_withObjects <- function(list[RSObject] array) RSMutableArray$new(objects)
RSMutableArray_withArray <- function(RSMutableArray array) array$mutableCopy()
RSMutableArray_withCapacity <- function(void) stop("Not Implemented.")

## RSDicitionary
RSDictionary_empty <- function(void) RSDictionary$new()
RSDictionary_withObjectForKey <- function(
  RSObject object,
  RSObject<RSCopying> key
) {
  RSDictionary$new(objects_and_keys = list(object, key))
}
RSDictionary_withObjectsForKeys <- function(
  list[RSObject] objects,
  list[RSObject<RSCopying>] keys
) {
  RSDictionary$new(keys = keys, values = objects)
}
RSDictionary_withObjectsAndKeys <- function(list[RSObject] objectsAndKeys) {
  indices <- seq_along(objectsAndKeys)
  values <- objectsAndKeys[seq_along %% 2L == 1L]
  keys   <- objectsAndKeys[seq_along %% 2L == 0L]
  stopifnot(
    "message" = # todo fill message
      length(objectsAndKeys) %% 2L == 0,
    "message" = # todo fill message
      all(vapply(objectsAndKeys, robjc_isBelongingRobjc, logical(1))),
    "message" = # todo fill message
      all(
        vapply(
          keys,
          function(key) key$conformsTo(RSCopying),
          logical(1)
        )
      )
  )
  RSDictionary$new(objects_and_keys = objectsAndKeys)
}
RSDictionary_withDictionary <- function(RSMutableDictionary dictionary) dictionary$copy()

## RSMutableDictionary
Mutable版

## RSSet
RSSet_empty <- function(void) RSSet$new()
RSSet_withObject <- function(RSObject obj) RSSet$new(list(obj))
RSSet_withObjects <- function(list[RSObject] objects) RSSet$new(objects)
RSSet_withArray <- function(RSArray array) RSSet$new(array$asList())
RSSet_withSet <- function(RSMutableSet set) set$copy()
