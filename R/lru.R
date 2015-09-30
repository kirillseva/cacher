LRUcache <- R6Class("LRUcache",
  public = list(
    initialize = function(size) {
      stopifnot(is.numeric(size))
      private$max_num = size
    },
    exists = function(name) {
      stopifnot(is.character(name) && length(name) == 1)
      name %in% ls(private$data)
    },
    set = function(name, value) {
      stopifnot(is.character(name) && length(name) == 1)
      if (length(private$data) == max_num && !(name %in% ls(private$data))) private$evict()
      private$save(name, value)
      invisible(self)
    },
    get = function(name) {
      stopifnot(is.character(name) && length(name) == 1)
      private$fetch(name)
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
    evict   = function() {

    }
  )
)
