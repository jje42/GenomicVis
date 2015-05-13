#' @import RColorBrewer
chromosome.palette <- function(chr, svs) {
  # Set up CTX chromosome colours
  sv.chr <- unique(c(svs$Chr1, svs$Chr2))
  sv.chr <- sv.chr[sv.chr != chr]
  n <- max(length(sv.chr), 3)   # minimum value for RColorBrewer is 3.
  pal <- if (n > 12)
    colorRampPalette(brewer.pal(12, 'Set3'))(n)
  else
    brewer.pal(n, 'Set3')
  names(pal) <- sv.chr
  pal
}
