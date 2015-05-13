#' Merge CNVs
#'
#' This is an internal helper function for \code{cnv.heatmap}.
#'
#' @param cnv cnv
#' @param samples samples
#' @return ?
#' @author Lutz Krause <lutz.krause@@qimrberghofer.edu.au>
cnv.merge <- function(cnv, samples) {
  cnv <- cnv[, c("Chr", "Begin", "End", samples)]
  df <- c("chr1", 0, 0, rep("Normal", length(samples)))
  current <- cnv[1, c("Chr", samples)]
  currentI <- 1
  n <- nrow(cnv)

  for (i in 2:n) {
    this <- cnv[i, c("Chr", samples)]
    if (!all(this == current)) {
      begin <- cnv[currentI, ]$Begin
      chr <- as.character(cnv[currentI, ]$Chr)
      end <- cnv[i - 1, ]$End
      df <- rbind(df, c(chr, begin, end, current[-1]))
      currentI <- i
      current <- this
    }
  }

  begin <- cnv[currentI, ]$Begin
  chr <- as.character(cnv[currentI, ]$Chr)
  end <- cnv[i - 1, ]$End
  df <- rbind(df, c(chr, begin, end, current[-1]))
  df <- df[-1, ]
  df <- data.frame(df)
  names(df) <- c("Chr", "Begin", "End", samples)
  df$Begin <- as.numeric(df$Begin)
  df$End <- as.numeric(df$End)
  df$Chr <- as.character(df$Chr)
  for (s in samples) 
    df[, s] <- as.character(df[, s])
  return(df)
}
