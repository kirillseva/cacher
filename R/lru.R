LRUcache_ <- R6::R6Class("LRUcache_",
  public = list(
    initialize = function(size) {
    },
    exists = function(name) {
      stopifnot(is.character(name) && length(name) == 1)
      name %in% ls(private$data)
    },
    set = function(name, value) {
      stopifnot(is.character(name) && length(name) == 1)
      private$save(name, value)
      invisible(self)
    },
    get = function(name) {
      stopifnot(is.character(name) && length(name) == 1)
      stopifnot(name %in% ls(private$data))
      private$fetch(name)
    },
    last_accessed = function(name) {
      stopifnot(is.character(name) && length(name) == 1)
      stopifnot(name %in% ls(private$data))
      private$peek(name)
    },
    print = function() {
      cat(sprintf("<LRUcache> of capacity %0.f%s", private$max_num, private$units),
          toString(private$format_cache())
          , sep="\n")
      invisible(self)
    }
  ),
  private = list(
    max_num = 0,
    data    = new.env(),
    units   = '',
    save    = function(name, value) {
      # Check if this value alone exceeds the cache size
      size <- private$get_new_item_size(value)
      if (size > private$max_num) {
        warning(sprintf("In package cacher: '%s' is too large (%0.f%s) to fit in the cache (%0.f%s) and will
          not be cached. Consider creating a larger cache.",
          name, size, private$units, private$max_num, private$units), call. = FALSE)
        return(NULL)
      }
      # Check for eviction
      while (private$check_for_eviction(name, size)) private$evict()
      private$data[[name]] <- list(value = value, timestamp = Sys.time())
    },
    fetch   = function(name) {
      private$data[[name]]$timestamp <- Sys.time()
      private$data[[name]]$value
    },
    peek    = function(name) {
      # Do not update the timestamp on peek
      private$data[[name]]$timestamp
    },
    evict   = function() {
      # This is very LRU specific. Evict the last used variable
      times  <- sapply(private$data, function(elem) elem$timestamp)
      oldest <- names(which.min(times))
      rm(list = oldest, envir = private$data)
    },
    format_cache = function() {
      format(plyr::ldply(as.list(private$data),
        .fun = function(x) { data.frame("timestamp" = x$timestamp, "value" = x$value) } ))
    },
    check_for_eviction = function(name, size) {
      private$get_current_size() + size > private$max_num && !(name %in% ls(private$data))
    }
  )
)

LRUcache_.numeric <- R6::R6Class("LRUcache_.numeric", inherit = LRUcache_,
  public = list(
    initialize = function(size) {
      stopifnot(all.equal(size, as.integer(size)) && length(size) == 1 && size > 0)
      private$max_num = size
    }
  ),
  private = list(
    get_new_item_size = function(item) 1,
    get_current_size = function() length(private$data)
  )
)

LRUcache_.character <- R6::R6Class("LRUcache_.character", inherit = LRUcache_,
  public = list(
    initialize = function(size) {
      # Parse size.  Check that string actually describes a size.
      stopifnot(length(regmatches(size, regexpr('[0-9.]+[kmg]?b?', size, ignore.case=TRUE))) > 0)
      private$max_num = private$convert_size_to_bytes(size)
    }
  ),
  private = list(
    units = 'B',
    get_new_item_size = function(item){
      as.numeric(as.character(pryr::object_size(item)))
    },
    convert_size_to_bytes = function(size){ # size is a string, e.g. "100mb"
      qty <- as.double(regmatches(size, regexpr("[0-9.]+", size))) # e.g. 100, 12.5
      units <- substring(toupper(regmatches(size, regexpr("[a-z]+", size, ignore.case=TRUE))), 1, 1) # e.g. "B","K","M", "G"...
      # Convert size to bytes
      pow <- switch(units,
        "B"=0,
        "K"=1,
        "M"=2,
        "G"=3)
      qty * (1000^pow) # pryr::object_size assumes 1KB = 1000B rather than 1024B
    },
    get_current_size = function(){ # in bytes
      if(length(private$data) == 0) 0 else
        Reduce(sum, lapply(private$data, function(x) { as.numeric(as.character(pryr::object_size(x))) }))
    }
  )
)
#' @export
LRUcache <- function(params) {
  UseMethod('LRUcache', params)
}

#' @export
LRUcache.numeric <- function(params) LRUcache_.numeric$new(params)
#' @export
LRUcache.character <- function(params) LRUcache_.character$new(params)
