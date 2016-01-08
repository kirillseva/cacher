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

  test_that('Test peek', {
    cache <- LRUcache(2)
    cache$set('hello', 'world')$set('cache', 'money')
    expect_is(cache$last_accessed('cache'), 'POSIXct')
    # did not update the timestamp metadata
    expect_true(cache$last_accessed('hello') < cache$last_accessed('cache'))
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

})
