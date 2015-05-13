#' Plot a SNV heatmap.
#'
#' description
#'
#' details
#'
#' @param x A \code{data.frame} with (at least) two columns name \code{Gene}
#' and \code{Tumour}.
#' @param width width
#' @param height height
#' @param margins margins
#' @param ... ?
#' @examples
#' set.seed(1000)
#' genes <- paste0('Gene_', toupper(letters)[1:20]) 
#' tumours <- paste0('LC1_', toupper(letters)[1:4])
#' x <- sapply(1:length(genes), 
#'   function(i) sample(tumours, sample(length(tumours))[1]))
#' names(x) <- genes
#' y <- as.data.frame(unlist(x))
#' dat <- data.frame(Gene = substr(rownames(y), 1, 6), Tumour = y[, 1])
#' snv.heatmap(dat, margins = c(5, 8))
#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @export
snv.heatmap <- function(x, width = 1000, height = 5000, margins = c(5, 12), ...) {
  ma <- t(as.matrix(table(as.character(x$Tumour), x$Gene)))
  sum <- apply(ma, 1, sum)
  oc <- apply(ma, 1, paste, collapse = ".")
  ma <- ma[order(paste(sum, oc), decreasing = TRUE), ]
  col <- matrix(rep(0, nrow(ma) * ncol(ma)), nrow = nrow(ma))
  plotSNVHeatmap(ma, margins = margins, ...)
  invisible(ma)
}

