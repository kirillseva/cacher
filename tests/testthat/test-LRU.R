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
})
