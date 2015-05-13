get.segments <- function(x, maxseg = NA, maxk = NA) {
  if (is.na(maxk))
    maxk <- 6
  if (is.na(maxseg)) {
    n <- length(x)
    maxseg <- ceiling(n / maxk)
  }
  seg <- tryCatch(
    tilingArray::segment(x$imd, maxseg = maxseg, maxk = maxk),
    error = function(e) {
      stop(sprintf('segment error: %s', e))
    })
  s <- c(1, seg@breakpoints[[maxseg]], length(x$imd))
  s
}
