context('test LRU cache')

describe('Bad initializers', {
  test_that('Cannot initialize without limit', {
    expect_error(LRUcache())
  })
  test_that('Cannot initialize with non-integer limit', {
    expect_error(LRUcache("hello"))
    expect_error(LRUcache(NULL))
    expect_error(LRUcache(1.5))
    expect_error(LRUcache(0))
    expect_error(LRUcache(-1))
  })
})

describe('Using LRU cache', {
  test_that('A small workflow', {
    cache <- LRUcache(2)
    expect_false(cache$exists('hello'))
    cache$set('hello', 'world')$set('cache', 'money')
    expect_true(cache$exists('hello'))
    expect_error(cache$exists(list(a=1)))
    expect_equal(cache$get('hello'), 'world')
    cache$set('goodbye', 'world')
    expect_false(cache$exists('cache'))
    expect_true(cache$exists('hello'))
  })

  test_that('Test last_accessed after set', {
    cache <- LRUcache(1)
    time <- Sys.time()
    cache$set('hello', 'world')
    expect_is(cache$last_accessed('hello'), 'POSIXct')
    expect_true(cache$last_accessed('hello') > time)
    expect_true(cache$last_accessed('hello') < Sys.time())
  })

  test_that('Test last_accessed after get', {
    cache <- LRUcache(1)
    cache$set('hello', 'world')
    time <- Sys.time()
    cache$get('hello')
    expect_is(cache$last_accessed('hello'), 'POSIXct')
    expect_true(cache$last_accessed('hello') > time)
    expect_true(cache$last_accessed('hello') < Sys.time())
  })

  test_that('Test last_accessed after peek', {
    cache <- LRUcache(1)
    cache$set('hello', 'world')
    time <- Sys.time()
    cache$peek('hello')
    expect_is(cache$last_accessed('hello'), 'POSIXct')
    expect_true(cache$last_accessed('hello') < time)
  })

  test_that('Test with byte size', {
    cache <- LRUcache('150B')
    cache$set('foo', 54.124)
    cache$set('bar', 54.124)
    cache$set('baz', 54.124)

    # foo should've been bumped due to size
    expect_false(cache$exists('foo'))

    # warning should raise with too large object
    expect_warning(cache$set('big', c(1:1e6)))
    expect_false(cache$exists('big'))
  })

  test_that('Test with large objects', {
    cache <- LRUcache('3GB')
    testthat::with_mock(
      `pryr::object_size` = function(...) "2147483648", # .Machine$integer.max + 1
      cache$set('foo', "some really big object"),
      expect_true(pryr::object_size("some really big object") == "2147483648"),
      expect_true(cache$exists('foo'))
      )
  })

  test_that('Different caches use different data stores', {
    cache1 <- LRUcache(10)
    cache2 <- LRUcache(10)
    cache1$set('foo', 'bar')
    expect_equal(cache1$get('foo'), 'bar')
    expect_false(cache2$exists('foo'))
  })

  test_that("It can forget a particular value", {
    cache <- LRUcache(10)
    cache$set("foo", "bar")
    expect_true(cache$exists("foo"))

    cache$forget("foo")
    expect_false(cache$exists("foo"))
  })

  test_that("It can produce a list of all cached values", {
    cache <- LRUcache(10)
    cache$set("foo", 1)
    cache$set("bar", 2)
    expect_true(setequal(cache$list_all(), c("foo", "bar")))
    cache$set("baz", 3)
  })

})
