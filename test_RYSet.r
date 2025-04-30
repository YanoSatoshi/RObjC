# test_RSSet.r
cat("=== RSSet TEST START ===\n")

x <- RSString$stringWith("x")
y <- RSString$stringWith("y")
z <- RSString$stringWith("z")

s1 <- RSSet$new(list(x, y))
s2 <- RSSet$new(list(y, x))
s3 <- RSSet$new(list(z))

# count / contains
stopifnot(s1$count() == 2)
stopifnot(s1$containsObject(x))
stopifnot(!s1$containsObject(RSString$stringWith("w")))

# isEqual
stopifnot(s1$isEqual(s2))
stopifnot(!s1$isEqual(s3))

# union
u <- s1$unionSet(s3)
stopifnot(u$count() == 3)
stopifnot(u$containsObject(x))
stopifnot(u$containsObject(z))

# intersection
i <- s1$intersectSet(s3)
stopifnot(i$count() == 0)

i2 <- s1$intersectSet(s2)
stopifnot(i2$isEqual(s1))

# difference
d <- s1$differenceSet(s2)
stopifnot(d$count() == 0)

d2 <- s1$differenceSet(s3)
stopifnot(d2$isEqual(s1))

# print
s1$print()

cat("=== RSSet TEST PASSED ===\n")
