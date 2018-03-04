test_that("`[[` allows retrieval of objects from cache", {
  x <- LRUcache(1)
  x$set("a", 10)
  expect_equal(x[["a"]], 10)
})

test_that("`[[` allows assignment of objects to cache", {
  y <- LRUcache(1)
  y[["b"]] <- 20
  expect_equal(y$get("b"), 20)
})
