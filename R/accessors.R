`[[.LRUcache_` <- function(cache, key) { cache$get(key) }
`[[<-.LRUcache_` <- function(cache, key, obj) { cache$set(key, obj) }
