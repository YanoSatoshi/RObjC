# RSHashOperator test

# Dummy data for testing
str1 <- RSString$new("A")
str2 <- RSString$new("B")
str3 <- RSString$new("C")
str4 <- RSString$new("A")  # isEqual to str1

helper <- RSHashOperatorr$module()

set1 <- list(str1, str2)
set2 <- list(str4, str2)  # logically same as set1
set3 <- list(str1, str3)  # different

cat("areEqualSets test 1 (should be TRUE):\n")
print(helper$areEqualSets(set1, set2))   # {'A','B'} == {*'A','B'}

cat("areEqualSets test 2 (should be FALSE):\n")
print(helper$areEqualSets(set1, set3))   # {'A','B'} == {'A','C'}

cat("objectsNotIn test1: \n")
print(sapply(helper$objectsNotIn(set1, set2), function(o) o$value())) # {'A','B'} %not.in% {*'A','B'}
cat("objectsNotIn test2: \n")
print(sapply(helper$objectsNotIn(set1, set3), function(o) o$value())) # {'A','B'} %not.in% {'A','C'}

cat("objectsAlsoIn test1: \n")
print(sapply(helper$objectsAlsoIn(set1, set2), function(o) o$value())) # {'A','B'} %in% {*'A','B'}
cat("objectsAlsoIn test2: \n")
print(sapply(helper$objectsAlsoIn(set1, set3), function(o) o$value())) # {'A','B'} %in% {'A','C'}



cat("unionOfSets test1: \n")
union_result <- helper$unionOfSets(set1, set2) # {'A','B'} cup {*'A','B'}
print(sapply(union_result, function(o) o$value()))
cat("unionOfSets test2: \n")
union_result <- helper$unionOfSets(set1, set3) # {'A','B'} cup {'A','C'}
print(sapply(union_result, function(o) o$value()))


cat("intersectionOfSets test1: \n")
intersect_result <- helper$intersectionOfSets(set1, set2) # {'A','B'} cap {*'A','B'}
print(sapply(intersect_result, function(o) o$value()))
cat("intersectionOfSets test2: \n")
intersect_result <- helper$intersectionOfSets(set1, set3) # {'A','B'} cap {'A','C'}
print(sapply(intersect_result, function(o) o$value()))


cat("differenceOfSets test1: \n")
diff_result <- helper$differenceOfSets(set1, set2) # {'A','B'} - {'A','B'}
print(sapply(diff_result, function(o) o$value()))
cat("differenceOfSets test2: \n")
diff_result <- helper$differenceOfSets(set1, set3) # {'A','B'} - {'A','C'}
print(sapply(diff_result, function(o) o$value()))
