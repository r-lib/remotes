
# TODO:
# - extract subset of files
# - hard links
# - device files
# - do not read in big files into memory
# - use seek, if stream is seeakable
# - restore mode
# - restore mtime
# - restore atime, ctime?
# - devices?

s1_untar <- local({

  # -- BUFFERED CONNECTION

  buffer <- local({

    tail <- function(x, n) {
      if (n == 0) return(x[FALSE])
      l <- length(x)
      if (n > 0) {
        if (n >= l) return(x)
        x[(l-n+1L):l]
      } else {
        n <- -n
        if (n >= l) return(x[FALSE])
        x[(n+1L):l]
      }
    }

    ## Buffered read from binary file
    buffer <- function(con, buffer_size = 512L * 1024L) {
      force(con)
      chunk_size <- buffer_size
      cache_con <- rawConnection(raw(0))

      ## Read out the full cache
      read_cache <- function(num_bytes) {
        ret <- readBin(cache_con, "raw", num_bytes)
        if (length(ret) < num_bytes) close(cache_con)
        ret
      }

      set_cache <- function(buf) {
        cache_con <<- rawConnection(buf)
      }

      err <- function(len, size, error) {
        if ((len < size && isTRUE(error)) ||
            (len < size && len != 0 &&
             identical(error, "partial-nonempty"))) {
          stop("Unexpected end of tar data")
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
          if (length(data) > size) set_cache(tail(data, -size))
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
          if (skipped > size) set_cache(tail(data, skipped - size))
          ret <- min(size, skipped)
          err(ret, size, error)
          ret
        },

        write_to = function(size, path, error = TRUE) {
          if (inherits(path, "connection")) {
            ocon <- path
          } else {
            ocon <- file(path, open = "wb")
            on.exit(close(ocon), add = TRUE)
          }

          data <- read_cache(size)
          writeBin(data, ocon)
          towrite <- size - length(data)
          while (towrite > 0) {
            data <- readBin(con, "raw", n = chunk_size)
            if (!length(data)) break
            writeBin(head(data, towrite), ocon)
            towrite <- towrite - min(towrite, length(data))
          }
          if (length(data) > towrite) set_cache(tail(data, -towrite))
          err(size - towrite, size, error)
          towrite
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
        stop("Invalid tar file, maybe corrupted?")
      }

      buf <- buf[-1]
      if (positive) {
        num <- as.integer(buf[buf != 0])
        as.integer(sum(rev(num) * 256^(1:length(num)-1L)))
      } else {
        num <- as.integer(buf)
        if (num[1] >= 0x80) num[1] <- num[1] - 0x80
        num <- 0xff - num
        - as.integer(sum(rev(num) * 256^(1:length(num)-1L)))
      }
    }

    decode_oct <- function(val, offset, length) {
      val <- val[offset:(offset+length-1)]
      if (val[1L] >= 0x80) return(parse256(val))

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

    nchar_bytes <- function(x) {
      nchar(x, type = "bytes")
    }

    decode <- function(buf, filename_encoding) {

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
      if (type_flag == 0 && substr(name, nchar_bytes(name), 1) == '/') {
        type_flag <- 5L
      }

      c = cksum(buf)

      # checksum is still initial value if header was null.
      if (c == 8 * 32) return(NULL)

      # valid checksum
      if (c != decode_oct(buf, 149, 8)) {
        stop("Invalid tar header. Maybe the tar file is corrupted")
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

  mkdirp <- function(path) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }

  check_safe_path <- function(path) {
    if (.Platform$OS.type == "windows") {
      path <- gsub("\\", "/", path, fixed = TRUE)
    }
    if (grepl("^\\.\\./|/\\.\\./|\\.\\.$", path)) {
      stop("Invalid path in tar file, contains `..`")
    }
  }

  safe_mkdirp <- function(dir, path) {
    check_safe_path(path)
    mkdirp(file.path(dir, path))
  }

  safe_symlink <- function(dir, path, linkname) {
    check_safe_path(path)
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
    buffer <- self$parser$read(512L, error = "partial-nonempty")
    if (!length(buffer)) return(FALSE)
    if (length(buffer) < 512L) stop("Unexpected end of tar file")

    header <- headers$decode(buffer, self$opts$filename_encoding)

    ## Maybe a null header? Try again...
    if (is.null(header)) return(TRUE)

    memo_types <- c("pax-header", "global-pax-header", "gnu-long-path")
    while (header$type %in% memo_types) {
      ## Pax global header
      if (header$type == "pax-global-header") {
        of <- overflow(header$size)
        buffer2 <- self$parser$read(header$size + of)
        process_pax_global_header(self, buffer2[1:header$size])
      }

      ## Pax header
      if (header$type == "pax-header") {
        of <- overflow(header$size)
        buffer2 <- self$parser$read(header$size + of)[1:header$size]
        process_pax_header(self, buffer2)
      }

      ## GNU long path
      if (header$type == "gnu-long-path") {
        of <- overflow(header$size)
        buffer2 <- self$parser$read(header$size + of)[1:header$size]
        process_gnu_long_path(self, buffer2)
      }

      buffer <- self$parser$read(512L, error = "partial-nonempty")
      if (!length(buffer)) return(FALSE)
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
        self$parser$skip(header$size + of)
      } else {
        self$parser$skip(header$size + of)
      }
      return(TRUE)
    }

    if (!header$size || header$type == "directory") {
      of <- overflow(header$size)
      if (self$mode == "extract") {
        safe_mkdirp(self$exdir, header$name)
        self$parser$skip(header$size + of)
      } else {
        self$parser$skip(header$size + of)
      }
      return(TRUE)
    }

    of <- overflow(header$size)
    if (self$mode == "extract") {
      check_safe_path(header$name)
      path <- file.path(self$exdir, header$name)
      mkdirp(dirname(path))
      withCallingHandlers({
        self$parser$write_to(header$size, path)
        self$parser$skip(of)
      }, error = function(e) unlink(path))
    } else {
      self$parser$skip(header$size + of)
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
      gname = map_str(items, "[[", "gname"),
      extra = I(lapply(items, function(x) as.list(x$pax)))
    )
  }

  map_str <- function (X, FUN, ...) {
    vapply(X, FUN, FUN.VALUE = character(1), ...)
  }

  map_int <- function(X, FUN, ...) {
    vapply(X, FUN, FUN.VALUE = integer(1), ...)
  }

  extract <- function(tarfile, exdir = ".",
                      options = list(filename_encoding = "")) {
    self <- new.env(parent = emptyenv())

    if (!inherits(tarfile, "connection")) {
      tarfile <- gzfile(tarfile, open = "rb")
      on.exit(close(tarfile), add = TRUE)
    }

    self$mode <- "extract"
    self$exdir <- exdir
    self$opts <- options
    self$parser <- buffer$buffer(tarfile)
    self$items <- list()

    repeat {
      if (!process_next_entry(self)) break;
    }

    make_result_df(self$items)
  }

  listx <- function(tarfile, options = list(filename_encoding = "")) {
    self <- new.env(parent = emptyenv())

    if (!inherits(tarfile, "connection")) {
      tarfile <- gzfile(tarfile, open = "rb")
      on.exit(close(tarfile), add = TRUE)
    }

    self$mode <- "list"
    self$opts <- options
    self$parser <- buffer$buffer(tarfile)
    self$items <- list()

    repeat {
      if (!process_next_entry(self)) break;
    }

    make_result_df(self$items)
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
