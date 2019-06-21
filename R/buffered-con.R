
# BUFFERED CONNECTION

buffer <- local({

  incomplete_file_error <- function() {
    structure(
      list(message = "Unexpected end of file/connection"),
      class = c("incomplete_file_error", "simpleError", "error",
                "condition")
    )
  }

  ## Buffered read from binary file
  buffer <- function(con, buffer_size = 1024L * 1024L * 4L) {
    force(con)
    chunk_size <- buffer_size
    cache_con <- rawConnection(readBin(con, "raw", buffer_size))

    ## Read out the full cache
    read_cache <- function(num_bytes) {
      readBin(cache_con, "raw", num_bytes)
    }

    set_cache <- function(buf) {
      base::close(cache_con)
      cache_con <<- rawConnection(buf)
    }

    err <- function(len, size, error) {
      if ((len < size && isTRUE(error)) ||
          (len < size && len != 0 &&
           identical(error, "partial-nonempty"))) {
        stop(incomplete_file_error())
      }
    }

    list(
      read = function(size, error = TRUE) {
        data <- read_cache(size)
        while (length(data) < size) {
          new <- readBin(con, "raw", n = chunk_size)
          if (!length(new)) break
          data <- c(data, new)
        }
        if (length(data) > size) set_cache(data[(size+1):length(data)])
        ret <- head(data, size)
        err(length(ret), size, error)
        ret
      },

      skip = function(size, error = TRUE) {
        if (size == 0) return(0)
        data <- read_cache(size)
        skipped <- length(data)
        while (skipped < size) {
          data <- readBin(con, "raw", n = chunk_size)
          if (!length(data)) break
          skipped <- skipped + length(data)
        }
        if (skipped > size) set_cache(data[(length(data)-skipped+size+1L):length(data)])
        ret <- min(size, skipped)
        err(ret, size, error)
        ret
      },

      write_to = function(size, path, error = TRUE) {
        if (inherits(path, "connection")) {
          ocon <- path
        } else {
          ocon <- file(path, open = "wb")
          on.exit(base::close(ocon), add = TRUE)
        }

        data <- read_cache(size)
        writeBin(data, ocon)
        written <- length(data)
        while (written < size) {
          data <- readBin(con, "raw", n = chunk_size)
          if (!length(data)) break
          towrite <- min(size - written, length(data))
          writeBin(head(data, towrite), ocon)
          if (towrite < length(data)) set_cache(data[(towrite+1):length(data)])
          written <- written + towrite
        }
        err(written, size, error)
        written
      },

      close = function() {
        try(base::close(cache_con), silent = TRUE)
        try(base::close(con), silent = TRUE)
      }
    )
  }

  structure(
    list(
      .internal = environment(),
      buffer = buffer
    ),
    class = c("standalone_buffer", "standalone")
  )
})
