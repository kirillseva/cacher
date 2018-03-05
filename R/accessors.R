#' @export
`[[.LRUcache_` <- function(cache, key) { cache$get(key) }

#' @export
`[[<-.LRUcache_` <- function(cache, key, value) { cache$set(key, value = value) }
