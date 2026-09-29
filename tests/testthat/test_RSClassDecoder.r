# test_that("RSClassDecoder loads, resolves, and unpacks encoded data", {
#   load_dummies()
#   encoder <- RSClassEncoder$module()$refresh()
#   obj <- DummyClass$new()
#   encoder$encodeObject(obj)
#   encoded_dict <- encoder$freeze()
#   decoder <- RSClassDecoder$module()
#   decoder$loadEncodedData(encoded_dict)
#   # Prepare class map (RSDictionary) for resolving
#   class_map <- RSMutableDictionary$new()
#   class_map$setObjectForKey(
#     RSWrappedClass$new(DummyClass),
#     RSString$stringWith("DummyClass_1.2")
#   )
#   decoder$resolveClassesWithDictionary(class_map)
#   env <- new.env()
#   decoder$unpackTo(env)
#   expect_true(exists(obj$objectID(), envir = env))
#   regen <- get(obj$objectID(), envir = env)
#   expect_equal(obj$class()$classname, regen$class()$classname)
#   unload_dummies()
# })

# test_that("RSClassDecoder fails when class is missing in map", {
#   load_dummies()
#   encoder <- RSClassEncoder$module()$refresh()
#   obj <- DummyClass$new()
#   encoder$encodeObject(obj)
#   encoded_dict <- encoder$freeze()
#   decoder <- RSClassDecoder$module()
#   decoder$loadEncodedData(encoded_dict)
#   # Empty resolver dic
#   class_map <- RSMutableDictionary$new()
#   expect_error(
#     decoder$resolveClassesWithDictionary(class_map),
#     "Unresolved classIDs: DummyClass_1.2"
#   )
#   unload_dummies()
# })
