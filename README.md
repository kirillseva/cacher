# cacher [![codecov.io](http://codecov.io/github/kirillseva/cacher/coverage.svg?branch=master)](http://codecov.io/github/kirillseva/cacher?branch=master)[![Build Status](https://travis-ci.org/kirillseva/cacher.svg)](https://travis-ci.org/kirillseva/cacher)

In memory caches for R. Currently implements LRU cache with size parameter.

Create a cache:

```R
cache <- LRUcache("150mb")  # cache with 150mb of size
cache2 <- LRUcache("1GB")   # cache with 1GB of size
cache3 <- LRUcache(3)       # cache that can hold three items of any size (that fits in RAM)
```

```R
cache3$set("key", "this is your value")  # set a value at a particular key...
cache3$get("key")                        # ...and get that value back!
[1] "this is your value"

cache3$exists("key")                     # ...see if your key exists
[1] TRUE

cache3$set("key2", "hi")                 # but if you go over your size...
cache3$set("key3", "hello")
cache3$set("key4", "yo")
cache3$exists("key")                     # ...the oldest key will automatically go away.
[1] FALSE                                # oldness is determined on when the key was most
                                         # recently accessed, not when it was set.

cache3$exists("key4")
[1] TRUE
cache3$forget("key4")                    # you can also explicitly forget a certain key.
cache3$exists("key4")
[1] FALSE
```
