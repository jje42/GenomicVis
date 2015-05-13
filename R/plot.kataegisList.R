#' Create a kataegis "rainfall" plot.
#'
#' Create a kataegis "rainfall" plot.
#'
#' In its simplest form (\code{plot(x)}), this function produces a rainfall
#' plot on the current graphical device.  By default, the whole genome is
#' displayed.  This can be changed by providing either the \code{chr} parameter
#' (to show a specific chromosome) or the \code{chr}, \code{start} and
#' \code{end} parameters to show a specific region of a chromosome (note that
#' all three parameters must be provided).
#'
#' @param x a \code{kataegisList} object, usually created with the
#' \code{kataegis} function.
#' @param chr Optionally, only process this chromosome
#' @param start Optionally, only process from this start point (must be specify
#' together with \code{chr} and \code{end}).
#' @param end Optionally, only process until this point (must be specify
#' together with \code{chr} and \code{start}).
#' @param all Plot all types of SNVs, default only C>X and T>X are plotted.
#' @param alpha adjust the alpha level of the points
#' @param legend If TRUE display a figure legend
#' @param colours a function specifying the colours to use in the plot, see the
#' default value (\code{kataegis.colours}) for an example.
#' @param ... passed to \code{plot} function
#' @return invisibly return \code{data.frame} of SNVs plotted.
#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @examples 
#' \dontrun{
#' vcf.file <- system.file('extdata', 'example.vcf', package = 'GenomicVis')
#' x <- kategis(vcf.file)
#' main <- sprintf("Kataegis: %d", length(x$kataegis))
#' plot(x, main = main)
#' }
#' @keywords methods
#' @import GenomicRanges
plot.kataegisList <- function(x, 
			      chr = NA,
			      start = NA,
			      end = NA,
                              all = FALSE, 
                              alpha = 0.5, 
			      legend = TRUE,
			      colours = kataegis.colours, 
			      ...) {
  # If either start or end is not NA, then all chr, start and end must be
  # provided.  chr may be provided without start or end.

  if (!is.na(start) && (is.na(chr) || is.na(end)))
    stop("start may not be specified without chr and end")

  if (!is.na(end) && (is.na(chr) || is.na(start))) 
    stop("end may not be specified without chr and start")

  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))

  if (is.function(colours))
    colours <- colours()

  snv <- x$snvs

  if (!is.na(chr)) {
    if (!is.na(start)) {
      r <- GRanges(chr, IRanges(start = start, end = end))
      snv <- subsetByOverlaps(snv, r)
    } else {
      snv <- snv[seqnames(snv) == chr]
    }
  }

  chrs <- seqlevels(snv)
  snv$ID <- 1:length(snv)
  katN <- length(x$kataegis)
  snv <- snv[snv$imd > -1, ]
  imd <- snv$imd
  y <- log10(imd)

  extra <- list(...)
  if (is.null(extra$cex))
    cex <- 0.5
  else
    cex <- extra$cex

  if (legend)
    par(oma = c(2, 0, 0, 0))

  plot(snv$ID, log10(snv$imd), type = 'n',
    yaxt = 'n',
    ylab = "Intermuation Distance (bp)", 
    xlab = "Mutation Number", 
    cex = cex, 
    ...)
  
  if (is.null(extra$pch)) {
    pch.type = 19
  } else {
    pch.type = extra$pch
  }

  alpha.f = alpha

  func <- function(ref) {
    bases <- c('A', 'T', 'G', 'C')
    refN <- snv[snv$REF == ref, ]
    for (i in bases[-which(bases == ref)]) {
      tN <- refN[unlist(refN$ALT) == i, ]
      points(
	tN$ID, 
	log10(tN$imd), 
	col = adjustcolor(colours[[paste(ref, i, sep = '>')]], alpha.f = alpha.f),
	pch = pch.type, 
	cex = cex
      )
    }
  }

  func('C')
  func('T')
  if (all) {
    func('G')
    func('A')
  }

  # labels<-c(1,'',100,'','10K','','1M','')
  # labels <- expression(10^0, 10^1, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7)
  labels <- expression(0, 10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7)
  axis(2, at = 0:7, labels = labels, las = 1)
  axis(1)
  for (chr in chrs) {
    rows <- snv[seqnames(snv) == chr]
    if (length(rows) > 0) {
      id = max(rows$ID)
      abline(v = id, col = 'grey', lty = 3)
      # n <- sub("chr", "", chr)
      # text(id + 75, 0.2, n, col = "darkgrey", cex = 0.8)
    }
  }


  # Plotting a legend outside a figure in R, taken from
  # http://dr-k-lo.blogspot.com.au/2014/03/the-simplest-way-to-plot-legend-outside.html

  if (legend) {
    labels <- if (all) names(colours) else c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G")
    col <- adjustcolor(colours[labels], alpha.f = alpha.f)
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
    legend('bottom', legend = labels, fill = col, box.col = 'white', 
      bty='n', xpd = TRUE, inset=c(0, 0), ncol = 6)
  }


  invisible(snv)
}

