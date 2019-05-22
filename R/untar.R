
# TODO:
# - extract subset of files
# - hard links
# - GNU extensions
# - do not read in big files into memory
# - use seek, if stream is seeakable

s1_untar <- local({

  # -- HEADER -------------------------------------------------------------

  headers <- local({

    ZEROS <- "0000000000000000000"
    SEVENS <- "7777777777777777777"
    ZERO_OFFSET <- as.integer(charToRaw("0"))
    EQUALS <- as.integer(charToRaw("="))

    to_type <- function(flag) {
      switch(
        as.character(flag),
        "0" = "file",
        "1" = "link",
        "2" = "symlink",
        "3" = "character-device",
        "4" = "block-device",
        "5" = "directory",
        "6" = "fifo",
        "7" = "contiguous-file",
        "72" = "pax-header",
        "55" = "pax-global-header",
        "27" = "gnu-long-link-path",
        "28" = ,
        "30" = "gnu-long-path",
        NULL
      )
    }

    cksum <- function(block) {
      8 * 32 + sum(as.integer(block[1:148])) +
        sum(as.integer(block[157:512]))
    }

    # @param buf raw vector

    parse256 <- function(buf) {
      if (buf[1] == 0x80) {
        positive <- TRUE
      } else if (buf[1] == 0xFF) {
        positive <- FALSE
      } else {
        ## TODO: error?
        return(NULL)
      }

      zero <- FALSE
      tuple <- integer()
      tx <- 1L
      for (byte in rev(buf)) {
        if (positive) {
          tuple[tx] <- as.integer(byte)
        } else if (zero && byte == 0L) {
          ## zero, but it is already zero
        } else if (zero) {
          zero <- FALSE
          tuple[tx] <- 0x100 - as.integer(byte)
        } else {
          tuple[tx] <- 0xFF - as.integer(byte)
        }
        tx <- tx + 1
      }

      l <- length(tuple)
      s <- sum(tuple * 256^(1:l))

      positive ? sum : - sum
    }

    decode_oct <- function(val, offset, length) {
      val <- val[offset:(offset+length-1)]
      if (val[1L] >= 0x80) return(parse256())

      val <- val[val != 0 & val != 32]
      if (length(val)) strtoi(rawToChar(val), 8L) else 0L
    }

    decode_str <- function(val, offset, length, encoding) {
      str <- rawToChar(val[offset:(offset+length-1)])
      Encoding(str) <- encoding
      str
    }

    decode_long_path <- function(buf, encoding) {
      decode_str(buf, 0, length(buf), encoding)
    }

    decode_pax <- function(buf) {
      entries <- strsplit(rawToChar(buf), "\n", fixed = TRUE)[[1]]
      recs <- strsplit(entries, "=", fixed = TRUE)
      structure(
        names = sub("^[^ ]+ ", "", map_str(recs, "[[", 1)),
        lapply(recs, "[[", 2))
    }

    decode <- function(buf, filename_encoding = "") {

      type_flag <- if (buf[157] == 0) {
        0
      } else {
        as.integer(buf[157]) - ZERO_OFFSET
      }

      name <- decode_str(buf, 1, 100, filename_encoding)
      mode <- decode_oct(buf, 101, 8)
      uid <- decode_oct(buf, 109, 8)
      gid <- decode_oct(buf, 117, 8)
      size <- decode_oct(buf, 125, 12)
      mtime <- decode_oct(buf, 137, 12)
      type <- to_type(type_flag)
      linkname <- if (buf[158] != 0) {
        decode_str(buf, 158, 100, filename_encoding)
      }
      uname <- decode_str(buf, 266, 32, "")
      gname <- decode_str(buf, 298, 32, "")
      devmajor <- decode_oct(buf, 330, 8)
      devminor <- decode_oct(buf, 338, 8)

      if (buf[346]) {
        name <- paste0(
          decode_str(buf, 346, 155, filename_encoding), '/', name)
      }

      # to support old tar versions that use trailing / to indicate dirs
      if (type_flag == 0 && substr(name, nchar(name), 1) == '/') {
        type_flag <- 5L
      }

      c = cksum(buf)

      # checksum is still initial value if header was null.
      if (c == 8 * 32) return(NULL)

      # valid checksum
      if (c != decode_oct(buf, 149, 8)) {
        stop("Invalid tar header. Maybe the tar is corrupted or it ",
             "needs to be gunzipped?")
      }

      list(
        name = name,
        mode = mode,
        uid = uid,
        gid = gid,
        size = size,
        mtime = .POSIXct(mtime),
        type = type,
        linkname = linkname,
        uname = uname,
        gname = gname,
        devmajor = devmajor,
        devminor = devminor
      )
    }

    structure(
      list(
        .internal = environment(),
        decode_long_path = decode_long_path,
        decode_pax = decode_pax,
        decode = decode
      ),
      class = c("standalone_tar_headers", "standalone")
    )
  })

  # -- EXCTRACT -----------------------------------------------------------

  overflow <- function(size) {
    size <- bitwAnd(size, 511L)
    if (size) 512L - size else 0L
  }

  mixin_pax <- function(header, pax) {
    if (!is.null(pax$path)) header$name <- pax$path
    if (!is.null(pax$linkpath)) header$linkname <- pax$linkname
    if (!is.null(pax$size)) header$size <- as.integer(pax$size)
    header$pax <- pax
    header
  }

  ## Buffered read from binary file
  make_parser <- function() {
    chunk_size <- 512L * 1024L
    cache <- raw()
    ptr <- 0L

    function(con, size, skip = FALSE) {
      if (size == 0) return(raw())
      if (size > chunk_size) chunk_size <- size
      ## Our of data? Read some
      if (size > length(cache) - ptr) {
        cur <- if (ptr < length(cache)) {
          cache[(ptr+1):length(cache)]
        } else {
          raw()
        }
        out <- readBin(con, "raw", n = chunk_size)
        ## Cannot read? Return what we have
        n <- length(out)
        if (!n) {
          cache <<- raw()
          ptr <<- 0L
          return(cur)
        }

        out <- c(cur, out)
        n <- length(out)
        while (n < size) {
          new <- readBin(con, "raw", n = chunk_size - n)
          nn <- length(new)
          if (!nn) stop("Incomplete tar file?")
          out <- c(out, new)
          n <- n + nn
        }
        cache <<- out
        ptr <<- 0L
      }

      ret <- if (!skip) cache[(ptr+1):(ptr+size)]
      ptr <<- ptr + size
      ret
    }
  }

  mkdirp <- function(path) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }

  safe_mkdirp <- function(dir, path) {
    ## TODO: check if path is not going back
    mkdirp(file.path(dir, path))
  }

  safe_write_bin <- function(object, dir, path) {
    ## TODO: check if path is not going back
    fpath <- file.path(dir, path)
    mkdirp(dirname(fpath))
    writeBin(object, fpath)
  }

  safe_symlink <- function(dir, path, linkname) {
    ## TODO: check if path is not going back
    file.symlink(linkname, file.path(dir, path))
  }

  process_pax_global_header <- function(self, buffer) {
    self$pax_global <- headers$decode_pax(buffer)
  }

  process_pax_header <- function(self, buffer) {
    pax <- headers$decode_pax(buffer)
    self$pax <- modifyList(as.list(self$pax_global), pax)
  }

  process_gnu_long_path <- function(self, buffer) {
    self$gnu_long_path <-
      headers$decode_long_path(buffer, self$opts$filename_encoding)
  }

  process_next_entry <- function(self) {
    buffer <- self$parse(self$con, 512L)
    if (!length(buffer)) return(FALSE)

    header <- headers$decode(buffer, self$opts$filename_encoding)

    ## Maybe a null header? Try again...
    if (is.null(header)) return(TRUE)

    memo_types <- c("pax-header", "global-pax-header", "gnu-long-path")
    while (header$type %in% memo_types) {
      ## Pax global header
      if (header$type == "pax-global-header") {
        of <- overflow(header$size)
        buffer2 <- self$parse(self$con, header$size + of)[1:header$size]
        process_pax_global_header(self, buffer2)
      }

      ## Pax header
      if (header$type == "pax-header") {
        of <- overflow(header$size)
        buffer2 <- self$parse(self$con, header$size + of)[1:header$size]
        process_pax_header(self, buffer2)
      }

      ## GNU long path
      if (header$type == "gnu-long-path") {
        of <- overflow(header$size)
        buffer2 <- self$parse(self$con, header$size + of)[1:header$size]
        process_gnu_long_path(self, buffer2)
      }

      buffer <- self$parse(self$con, 512L)
      header <- headers$decode(buffer, self$opts$filename_encoding)
    }

    if (!is.null(self$gnu_long_path)) {
      header$name <- self$gnu_long_path
      self$gnu_long_path <- NULL
    }

    if (!is.null(self$pax)) {
      header <- mixin_pax(header, self$pax)
      self$pax <- NULL
    }

    self$items[[length(self$items) + 1L]] <- header

    if (header$type == "symlink") {
      of <- overflow(header$size)
      if (self$mode == "extract") {
        safe_symlink(self$exdir, header$name, header$linkname)
        self$parse(self$con, header$size + of)
      } else {
        self$parse(self$con, header$size + of, skip = TRUE)
      }
      return(TRUE)
    }

    if (!header$size || header$type == "directory") {
      of <- overflow(header$size)
      if (self$mode == "extract") {
        safe_mkdirp(self$exdir, header$name)
        self$parse(self$con, header$size + of)
      } else {
        self$parse(self$con, header$size + of, skip = TRUE)
      }
      return(TRUE)
    }

    of <- overflow(header$size)
    if (self$mode == "extract") {
      cnt <- self$parse(self$con, header$size + of)
      safe_write_bin(cnt, self$exdir, header$name)
    } else {
      self$parse(self$con, header$size + of, skip = TRUE)
    }

    return(TRUE)
  }

  make_result_df <- function(items) {
    data.frame(
      stringsAsFactors = FALSE,
      filename = map_str(items, "[[", "name"),
      size = map_int(items, "[[", "size"),
      mtime = .POSIXct(vapply(items, "[[", .POSIXct(1), "mtime")),
      permissions = I(as.octmode(map_int(items, "[[", "mode"))),
      type = map_str(items, "[[", "type"),
      uid = map_int(items, "[[", "uid"),
      gid = map_int(items, "[[", "gid"),
      uname = map_str(items, "[[", "uname"),
      gname = map_str(items, "[[", "gname")
    )
  }

  extract_connection <- function(con, exdir, options) {

    self <- new.env(parent = emptyenv())

    self$mode <- "extract"
    self$con <- con
    self$exdir <- exdir
    self$opts <- options
    self$parse <- make_parser()
    self$items <- list()

    repeat {
      if (!process_next_entry(self)) break;
    }

    make_result_df(self$items)
  }

  map_str <- function (X, FUN, ...) {
    vapply(X, FUN, FUN.VALUE = character(1), ...)
  }

  map_int <- function(X, FUN, ...) {
    vapply(X, FUN, FUN.VALUE = integer(1), ...)
  }

  list_connection <- function(con, options) {
    self <- new.env(parent = emptyenv())
    self$mode <- "list"
    self$con <- con
    self$opts <- options
    self$parse <- make_parser()
    self$items <- list()

    repeat {
      if (!process_next_entry(self)) break;
    }

    make_result_df(self$items)
  }

  extract <- function(tarfile, exdir = ".",
                      options = list(filename_encoding = "")) {
    if (inherits(tarfile, "connection")) {
      extract_connection(tarfile, exdir, options)
    } else {
      con <- file(tarfile, open = "rb")
      on.exit(close(con), add = TRUE)
      extract_connection(con, exdir, options)
    }
  }

  listx <- function(tarfile, options = list(filename_encoding = "")) {
    if (inherits(tarfile, "connection")) {
      list_connection(tarfile, options)
    } else {
      con <- file(tarfile, open = "rb")
      on.exit(close(con), add = TRUE)
      list_connection(con, options)
    }
  }

  # -- EXPORTED API -------------------------------------------------------

  structure(
    list(
      .internal = environment(),
      list = listx,
      extract = extract
    ),
    class = c("standalone_tar", "standalone")
  )
})
