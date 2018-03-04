`[[` <- function(cache, key) { UseMethod("[[") }

`[[.default` <- .Primitive("[[")

`[[.LRUcache_` <- function(cache, key) { cache$get(key) }



`[[<-` <- function(cache, key, obj) { UseMethod("[[<-") }

`[[<-.default` <- .Primitive("[[<-")

`[[<-.LRUcache_` <- function(cache, key, obj) { cache$set(key, obj) }
