LRUcache_class <- R6::R6Class("LRUcache",
  public = list(
    initialize = function(size) {
      # Parse size
      if(is.character(size)){
        # Check that string actually describes a size
        stopifnot(length(regmatches(size,regexpr('[0-9.]+[kmg]?b?',size, ignore.case=TRUE))) > 0)
        private$max_num = private$convert_size_to_bytes(size)
        private$use_bytes = TRUE
      } else {
        stopifnot(all.equal(size, as.numeric(size)) && length(size) == 1 && size > 0)
        private$max_num = size
      }
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
    }
  ),
  private = list(
    max_num = 0,
    data    = new.env(),
    use_bytes = FALSE,
    save    = function(name, value) {
      # Check if this value alone exceeds the cache size
      size <- if(private$use_bytes) as.integer(as.character(pryr::object_size(value))) else 1
      if (size > private$max_num){
        warning(sprintf("In package cacher: '%s' is too large (%dB) to fit in the LRU cache (%dB) and will not be cached. Consider creating a larger cache.", name, size, private$max_num), call. = FALSE)
        return(NULL)
      }
      # Check for eviction
      while (private$get_current_size() + size > private$max_num && !(name %in% ls(private$data))) private$evict()
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
    convert_size_to_bytes = function(size){ # size is a string, e.g. "100mb"
      qty <- as.double(regmatches(size, regexpr("[0-9.]+", size))) # e.g. 100, 12.5
      units <- substring(toupper(regmatches(size, regexpr("[a-z]+", size, ignore.case=TRUE))),1,1) # e.g. "B","K","M", "G"...
      # Convert size to bytes
      pow <- switch(units,
        "B"=0,
        "K"=1,
        "M"=2,
        "G"=3)
      qty * (1000^pow) # pryr::object_size assumes 1KB = 1000B rather than 1024B
    },
    get_current_size = function(){ # in bytes
      if(length(private$data) == 0){
        0
      } else if(private$use_bytes){
        Reduce(sum, lapply(private$data, function(x) {as.numeric(as.character(pryr::object_size(x)))}))
      } else {
        length(private$data)
      }
    }
  )
)

#' @export
LRUcache <- function(size) LRUcache_class$new(size)
