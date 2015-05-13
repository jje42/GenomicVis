#' Illustrative function to read GAP output.
#'
#' This function is provided as an example of reading output from GAP and
#' creating the required \code{data.frame} for \code{cnv.plot}.
#'
#' @param file GAP output file
#' @param annot GAP output annotation file
#' @param sample.id The name of the sample to process
#' @return A \code{data.frame} instance that can be used as input for
#' \code{cnv.plot}.
#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @examples
#' \dontrun{
#' read.gap()
#' }
#' @import data.table
#' @export
read.gap <- function(file, annot, sample.id = NULL) {
  annot <- fread(annot)
  setnames(annot, c('X', 'Index', 'Chr_N', 'Chr_arm', 'Position'))
  setkey(annot, 'Index')

  if (ncol(annot) != 5)
    stop("ERROR: unknown format for annotation file")

  gap <- fread(file)

  # GAP output has chromosome names like 24.5!  Why is this?
  # GAP numbers all chromosomes so this column will be read in as a numeric,
  # this will cause problems later so convert in to a character.
  gap$Chr <- as.character(tryCatch(floor(gap$Chr), error = function(e) gap$Chr))

  if (!all(gap$IndxSnpEnd %in% annot$Index))
    stop("there are IndxSnpEnd's that do not have a corresponding Index")

  if (! all(annot[annot$Index %in% gap$IndxSnpEnd, ]$Chr_N == gap$Chr))
    stop("Chromosome mismatch (End)")

  if (! all(annot[annot$Index %in% gap$IndxSnpEnd, ]$Chr_N == gap$Chr))
   stop("Chromosome mismatch (Start)")

  gap$Begin <- annot[.(gap$IndxSnpStart)]$Position
  gap$End <- annot[.(gap$IndxSnpEnd)]$Position

  gap <- gap[gap$Chr %in% 1:24, ]

  # This use to be gap2segments() ...
  if (!is.null(sample.id)) {
    sample.id <- make.names(sample.id)
    copy <- paste0("CN_", sample.id)
    mallele <- paste0("BA_", sample.id)
    if (! copy %in% names(gap))
      stop("ERROR ", copy, " not defined for gap: ", paste(names(gap), sep = " ",
	  collapse=" "))
    if (! mallele %in% names(gap))
      stop("ERROR ", mallele, " not defined for gap: ", paste(names(gap),
	  sep = " ", collapse = " "))
    gap$Copy <- gap[, copy, with = FALSE]
    gap$Mallele <- gap[, mallele, with = FALSE]
  }

  # GAP likes to number chromosomes from 1 to 24.  Rename everything to
  # chr1, ..., chrY etc.

  if (any(gap$Chr == 23)) 
    gap[gap$Chr == 23, ]$Chr <- "X"
  if (any(gap$Chr == 24)) 
    gap[gap$Chr == 24, ]$Chr <- "Y"

  gap$Chr <- paste0('chr', gap$Chr)

  gap
}
