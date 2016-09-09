read_dcf <- function(path) {
  fields <- colnames(read.dcf(path))
  as.list(read.dcf(path, keep.white = fields)[1, ])
}

write_dcf <- function(path, desc) {
  write.dcf(
    rbind(unlist(desc)),
    file = path,
    keep.white = names(desc),
    indent = 0
  )
}

get_desc_field <- function(path, field) {
  dcf <- read_dcf(path)
  dcf[[field]]
}
