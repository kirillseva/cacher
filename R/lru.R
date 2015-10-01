LRUcache_class <- R6::R6Class("LRUcache",
  public = list(
    initialize = function(size) {
      stopifnot(all.equal(size, as.integer(size)) && length(size) == 1 && size > 0)
      private$max_num = size
    },
    exists = function(name) {
      stopifnot(is.character(name) && length(name) == 1)
      name %in% ls(private$data)
    },
    set = function(name, value) {
      stopifnot(is.character(name) && length(name) == 1)
      if (length(private$data) == private$max_num && !(name %in% ls(private$data))) private$evict()
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
    }
  ),
  private = list(
    max_num = 0,
    data    = new.env(),
    save    = function(name, value) {
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
    }
  )
)

#' @export
LRUcache <- function(size) LRUcache_class$new(size)
