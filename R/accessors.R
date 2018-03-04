`[[.LRUcache_` <- function(cache, key) { cache$get(key) }
`[[<-.LRUcache_` <- function(cache, key, value) {
  cache$set(key, value = value)
}
