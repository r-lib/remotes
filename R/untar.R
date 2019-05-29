
# ## Advantages over `utils::untar()`
#
# - Typically 2-3 times faster.
# - `list()` and `extract()` both return a data frame with data about the
#   contents, including all pax data.
# - Errors if file would be extracted outside of the target directory.
# - Creates junction points for symbolic links on Windows.
# - It does not leave incomplete files around if interrupted or if the tar
#   file is imcomplete.
# - Defaults to UTF-8 encoding of filenames and pax metadata, but supports
#   other encodings as well, is they are specified explicitly.
# - Supports (extended) glob patterns instead of regular expressions, to
#   specify files and directories to extract.
# - Supports large files.
# - Supports global pax headers.
# - Has `overwrite = FALSE` option.
# - Has a `limit` option to look at or extarct the first couple of
#   files (that match a glob pattern).
#
# ## Disadvantages compared to `utils::untar()`
#
# - `utils::untar()` tries to create copies if links don't work on Windows.
# - `utils::untar()` supports uncompression directly.
# - `utils::untar()` has an option to avoid restoring mtime.
#
# ## NEWS
#
# ### 2019-05-29 First version
#
# ## Roadmap
#
# - check extracting to symlinks to avoid overwriting files
# - autodetect zip, bzip2 and xz compressed files
# - autodetect compressed connections (hard)
# - extract into memory
# - handle resource forks on macOS
# - hard links
# - better behavior of links on Windows
# - device files
# - restore atime, ctime?
# - -k, do not overwrite files
# - --keep-newer-files, do not overwrite files that are newer
# - -m do not extract modification time
# - -p preserve file permissions
# - --strip-components

s1_untar <- local({

  # -- HEADER -------------------------------------------------------------

  # These are other standalone files. They happen to be before us in the
  # collate order...
  buffer <- buffer
  glob <- glob

  headers <- local({

    ZERO_OFFSET <- as.integer(charToRaw("0"))

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

    decode_pax <- function(buf, encoding) {
      entries <- strsplit(rawToChar(buf), "\n", fixed = TRUE)[[1]]
      recs <- strsplit(entries, "=", fixed = TRUE)
      structure(
        names = sub("^[^ ]+ ", "", map_chr(recs, "[[", 1)),
        lapply(lapply(recs, "[[", 2), `Encoding<-`, encoding))
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
      uname <- decode_str(buf, 266, 32, filename_encoding)
      gname <- decode_str(buf, 298, 32, filename_encoding)
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

  set_file_metadata <- function(path, header) {
    Sys.chmod(path, header$mode, FALSE)
    Sys.setFileTime(path, header$mtime)
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

  do_mkdirp <- function(path, header) {
    mkdirp(path)
    set_file_metadata(path, header)
  }

  if (.Platform$OS.type != "windows") Sys.junction <- function(...) stop()

  do_symlink <- function(path, header, exdir) {
    mkdirp(dirname(path))
    link <- if (.Platform$OS.type == "windows") Sys.junction else file.symlink
    link(file.path(exdir, header$linkname), path)
    set_file_metadata(path, header)
  }

  process_pax_global_header <- function(self, buffer) {
    self$pax_global <- headers$decode_pax(buffer, self$opts$filename_encoding)
  }

  process_pax_header <- function(self, buffer) {
    pax <- headers$decode_pax(buffer, self$opts$filename_encoding)
    self$pax <- modifyList(as.list(self$pax_global), pax)
  }

  process_gnu_long_path <- function(self, buffer) {
    self$gnu_long_path <-
      headers$decode_long_path(buffer, self$opts$filename_encoding)
  }

  incomplete_file_error <- function() {
    structure(
      list(message = "Unexpected end of file/connection"),
      class = c("incomplete_file_error", "simpleError", "error",
                "condition")
    )
  }

  any_matches <- function(patterns, x) {
    m <- matrix(
      as.logical(unlist(lapply(patterns, grepl, x = x))),
      nrow = length(x))
    apply(m, 1, any)
  }

  all_dirnames <- function(path) {
    pos <- gregexpr("/", path, fixed = TRUE)[[1L]] - 1L
    px <- substring(path, 1, pos)
    px[px != ""]
  }

  process_next_entry <- function(self) {
    if (self$next_item == self$opts$limit) return(FALSE)

    buffer <- self$parser$read(512L, error = "partial-nonempty")
    if (!length(buffer)) return(FALSE)
    if (length(buffer) < 512L) stop(incomplete_file_error())

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

    skip_this <- FALSE
    skip_list <- FALSE
    if (!is.null(self$patterns)) {
      names <- c(all_dirnames(header$name), header$name)
      if (!any(any_matches(self$patterns, names))) {
        skip_this <- skip_list <- TRUE
      }
    }

    if (!skip_list) {
      self$next_item <- self$next_item + 1L
      self$items[[as.character(self$next_item)]] <- header
    }

    if (self$mode != "extract") {
      skip_this <- TRUE
    } else {
      path <- file.path(self$exdir, header$name)
      if (file.exists(path) && !self$opts$overwrite) {
        skip_this <- TRUE
        message("*Not* overwriting path `", header$name, "`")
      }
    }

    of <- overflow(header$size)
    if (skip_this) {
      self$parser$skip(header$size + of)
      return(TRUE)
    }

    ## We only do this here, so the path can be included in the list
    check_safe_path(header$name)

    if (header$type == "symlink") {
      if (self$mode == "extract") do_symlink(path, header, self$exdir)
      self$parser$skip(header$size + of)
      return(TRUE)
    }

    if (!header$size || header$type == "directory") {
      if (self$mode == "extract") do_mkdirp(path, header)
      self$parser$skip(header$size + of)
      return(TRUE)
    }

    if (self$mode == "extract") {
      self$extracting <- path
      mkdirp(dirname(path))
      self$parser$write_to(header$size, path)
      self$parser$skip(of)
      self$extracting <- NULL
      set_file_metadata(path, header)
    } else {
      self$parser$skip(header$size + of)
    }

    return(TRUE)
  }

  make_result_df <- function(items, num) {
    items <- unname(mget(as.character(seq_len(num)), items))
    data.frame(
      stringsAsFactors = FALSE,
      filename = map_chr(items, "[[", "name"),
      size = map_int(items, "[[", "size"),
      mtime = .POSIXct(vapply(items, "[[", .POSIXct(1), "mtime")),
      permissions = I(as.octmode(map_int(items, "[[", "mode"))),
      type = map_chr(items, "[[", "type"),
      uid = map_int(items, "[[", "uid"),
      gid = map_int(items, "[[", "gid"),
      uname = map_chr(items, "[[", "uname"),
      gname = map_chr(items, "[[", "gname"),
      extra = I(lapply(items, function(x) as.list(x$pax)))
    )
  }

  map_chr <- function (X, FUN, ...) {
    vapply(X, FUN, FUN.VALUE = character(1), ...)
  }

  map_int <- function(X, FUN, ...) {
    vapply(X, FUN, FUN.VALUE = integer(1), ...)
  }

  process_file <- function(self, tarfile, patterns, options) {
    filesize <- NA_integer_

    if (!inherits(tarfile, "connection")) {
      filesize <- file.size(tarfile)
      tarfile <- gzfile(tarfile, open = "rb")
      on.exit(close(tarfile), add = TRUE)
    }

    chunk_size <- min(filesize, 1024L * 1024L * 256L, na.rm = TRUE)

    if (!is.null(patterns)) {
      self$patterns <- map_chr(patterns, glob$to_regex)
    }
    self$opts <- utils::modifyList(default_options(), options)
    self$parser <- buffer$buffer(tarfile, chunk_size)
    on.exit(self$parser$close(), add = TRUE)

    self$items <- new.env(parent = emptyenv(), size = 5939)
    self$next_item <- 0L

    tryCatch(
      repeat {
        if (!process_next_entry(self)) break
      },
      incomplete_file_error = function(e) {
        e$message <- "Unexpected end of tar data file/connection"
        e$processed <- TRUE
        stop(e)
      },
      error = function(e) {
        if (!is.null(self$extracting)) {
          tryCatch(unlink(self$extracting), error = function(e) NULL)
        }
        if (isTRUE(e$processed)) stop(e)
        msg <- paste("Failed to decode tar data, maybe file is corrupt?",
                     conditionMessage(e))
        err <- structure(list(message = msg, parent = e),
                         class = c("simpleError", "error", "condition"))
        stop(err)
      }
    )

    make_result_df(self$items, self$next_item)
  }

  extract <- function(tarfile, exdir = ".", patterns = NULL,
                      options = list()) {
    self <- new.env(parent = emptyenv())
    self$mode <- "extract"
    self$exdir <- exdir
    process_file(self, tarfile, patterns, options)
  }

  listx <- function(tarfile, patterns = NULL, options = list()) {
    self <- new.env(parent = emptyenv())
    self$mode <- "list"
    process_file(self, tarfile, patterns, options)
  }

  default_options <- function() {
    list(
      filename_encoding = "UTF-8",
      overwrite = TRUE,
      limit = Inf
    )
  }

  # -- EXPORTED API -------------------------------------------------------

  structure(
    list(
      .internal = environment(),
      list = listx,
      extract = extract,
      default_options = default_options
    ),
    class = c("standalone_tar", "standalone")
  )
})
