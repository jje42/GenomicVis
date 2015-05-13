#' Illustrative function for processing GAP data.
#'
#' This function is an example of reading data output by GAP, and using it
#' as input for the \code{cnv.heatmap} function.
#' 
#' @param gap A \code{data.frame}, typically obtained by calling
#' \code{read.gap}.
#' @return A \code{data.frame} suitable as input to the \code{cnv.heatmap}
#' function.
#' @author Lutz Krause <lutz.krause@@qimrberghofer.edu.au>
#' @examples
#' \dontrun{
#' cnv <- gap2cnv(gap)
#' }
#' @export
gap2cnv <- function(gap) {
  names <- names(gap)
  samples <- names[grepl("CN_", names)]
  samples <- sub("CN_", "", samples)
  cnv <- data.frame(
    Chr = as.character(gap$Chr), 
    Begin = gap$Begin, 
    End = gap$End, 
    stringsAsFactors = FALSE
  )
  n <- nrow(cnv)
  for (sample in samples) {
    cn <- paste0("CN_", sample)
    cn <- gap[, cn, with = FALSE]
    ba <- paste0("BA_", sample)
    ba <- gap[, ba, with = FALSE]
    con <- rep("Normal", n)
    con[cn == 1] <- "Loss"
    con[cn == 0] <- "HD"
    con[cn > 2] <- "Gain"
    con[cn > 6] <- "Amplified"
    con[cn == 2 & ba != 1] <- "NLOH"
    cnv[, sample] <- con
  }
  return(cnv)
}
