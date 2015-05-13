#' Create a CNV heatmap.
#'
#' description 
#'
#' The \code{cnv} parameter should be a \code{data.frame} describing the
#' CNVs for a cohort of samples. It must have \code{Chr}, \code{Begin} and
#' \code{End} columns and additional columns for each sample.  The sample
#' columns should contain character descriptions relating to the CNV
#' described by the \code{Chr}, \code{Begin} and \code{End}. The package
#' provides an example data set, \code{CNVData}, that classifies each CNV as
#' either Normal, Gain, Loss, Amplified, NLOH or HD.  However, you are free
#' to use your own classifications, but be sure to set \code{colour.map}
#' accordingly.
#'
#' The \code{GRanges} object can be built by any means, but we have found it
#' convenient, particularly for human data, to build this parameter from
#' Bioconductor packages.  Specifically, the
#' \pkg{TxDb.Hsapiens.UCSC.hg19.knownGene} and \pkg{org.Hs.eg.db} packages.
#' An example of how this can be done is given in the Examples section
#' below. If the \pkg{TxDb.Hsapiens.UCSC.hg19.knownGene} package does not
#' suit your needs and there is no other available \code{TxDb} package, it
#' is very simple to build custom \code{TxDb} packages, see the
#' documentation for the \pkg{GenomicFeatures} package for further details.
#'
#' @param cnv A \code{data.frame} describing the CNVs for a cohort of
#' samples. It must have \code{Chr}, \code{Begin} and \code{End} columns and
#' additional columns for each sample.  The sample columns should contain
#' character descriptions relating to the CNV described by the \code{Chr},
#' \code{Begin} and \code{End}.  See the Details section for further
#' information.
#' @param samples A character vector specifying the samples to use in the
#' heatmap.  If no samples are provided the function will use all samples as
#' determined by taking the column names after the third position (the first
#' three columns should be chromosome, begin and end).
#' @param minBP minBP
#' @param symbols A character vector of gene symbols.  If provided you must
#' also provide the \code{genes.gr} parameter, and the symbols must
#' correspond to the \code{symbol} column in the \code{gene.gr} instance.
#' See the Details section for further information.
#' @param genes.gr A \code{GRanges} instance containing the locations of
#' genes.  The chromosome names (\code{seqnames}) must match the chromosome
#' names used in the CNV data.  The \code{GRanges} instance must also
#' contain a \code{symbol} column in its \code{mcols} attribute.  See the
#' Details section for further details and the examples section for a way to
#' build this parameter using Bioconductor packages.
#' @param colour.map Colour scheme to use for the heatmap.  It should be a
#' \code{data.frame} with the first column specifying the CNV categories and
#' the second column specifying the colour to use.
#' @return ???
#' @examples
#' \dontrun{
#' # The genes.gr argument can easily be built from available Bioconductor
#' # packages.  The following code shows how you could build the appropriate
#' # GRanges instance for human hg19 data.
#' library(TxDb.Hsapiens.UCSC.hg19.knownGene)
#' library(org.Hs.eg.db)
#' genes.gr <- genes(TxDb.Hsapiens.UCSC.hg19.knownGene)
#' gene_ids <- unlist(genes.gr$gene_id)
#' symbol.map <- select(org.Hs.eg.db, gene_ids, 'SYMBOL')
#' genes.gr$symbol <- symbol.map$SYMBOL
#' }
#' 
#' # The data set hg19Genes contains human genes with the
#' #appropriate gene symbols.
#' data(hg19Genes)
#'
#' data(CNVData)
#' set.seed(100)
#' g <- sample(hg19Genes$symbol, 20)
#' cnv.heatmap(CNVData, symbols = g, genes.gr = hg19Genes)
#' 
#' \dontrun{
#' # To just use a subset of samples.
#' cnv.heatmap(CNVData, samples = c('LC3_A', 'LC3_B'), 
#'   genes.gr = hg19Genes)
#' }
#' @author Lutz Krause <lutz.krause@@qimrberghofer.edu.au>
#' @export
cnv.heatmap <- function(cnv, 
                        samples = NA, 
			minBP = 10^6, 
			symbols = NULL, 
                        genes.gr = NULL,
			colour.map = default.cnv.heatmap.colour.map()) {

  if (!is.null(symbols) & is.null(genes.gr))
    stop("cnv.heatmap: if you supply 'symbols', you must also supply 'genes.gr'")

  if (is.na(samples)) 
    samples <- colnames(cnv)[4:ncol(cnv)]

  cnv <- cnv.merge(cnv, samples)
  cnv$Length <- cnv$End - cnv$Begin
  cnv <- cnv[cnv$Length > minBP, ]
  tmp <- cnv == "Normal"
  sum <- apply(tmp, 1, sum)
  cnv <- cnv[sum < length(samples), ]
  yaxis <- FALSE

  if (!missing(symbols)) {
    cnv$Genes <- genes.in.region(cnv$Chr, cnv$Begin, cnv$End, genes.gr)
    new <- cnv[1, ]
    new$Symbol <- ""
    genes <- cnv$Genes

    for (s in symbols) {
      regexp <- paste0(s, ";|", s, "$")
      tmp <- cnv[grepl(regexp, genes), ]
      if (dim(tmp)[1] > 0) {
        sym <- s
        if (dim(tmp)[1] > 1) 
	  sym <- paste(s, 1:dim(tmp)[1])
        tmp$Symbol <- sym
        new <- rbind(new,tmp)
      }
    }
    yaxis <- TRUE
    rownames(new) <- new$Symbol
    new <- new[-1, ]
    new <- new[-1, ]
    cnv <- new
  }

  cnv <- cnv[, samples]
  width <- 600
  if (yaxis) 
    width <- 1200
  height <- nrow(cnv) * 80 + 500
  # if (!missing(png)) 
  #   png(png, width = width, height = height, res = 300)
  heatmap.categorical(cnv, colormap = colour.map, yaxis = yaxis)
  # if (! missing(png)) 
  #   dev.off()

  invisible(cnv)
}
