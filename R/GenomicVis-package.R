#' Genomic Visualisation 
#'
#' The \pkg{GenomicVis} package contains a collection of function for the
#' visualisation of genomic data typically derived from high-throughput
#' whole genome sequencing and SNP-Chip experiments.  Most of the functions
#' provided by this package are agnostic about where the data actually comes
#' from, that is, they required input in simple \code{data.frame} instances
#' that are not tied to a particular file format; however, we also provide
#' many examples of creating these simply \code{data.frame}s from standard
#' file formats such as VCFs and from common software tools such as GAP and
#' Breakdancer.
#' 
#' For further details, and examples, users are directed to the package
#' vignette: run \code{vignette('GenomicVis')} from the R prompt.
#'
#' @docType package
#' @name GenomicVis
NULL

#' An example of output from the \code{kataegis} function.
#'
#' This dataset serves as an example of the output of the \code{kataegis}
#' function, and it can be used for creating kataegis rainfall plots.
#'
#' @docType data
#' @keywords datasets
#' @format A data frame with x rows and y variables
#' @name KataegisExample
NULL

#' An example of a CNV data frame used by \code{cnv.heatmap}.
#'
#' @docType data
#' @keywords datasets
#' @name CNVData
NULL

#' An example of a CNV data frame used by \code{cnv.plot}.
#'
#'
#' @docType data
#' @keywords datasets
#' @name CNVExample
NULL

#' An example of a SNP data frame used by \code{cnv.plot}.
#'
#' @docType data
#' @keywords datasets
#' @name SNPExample
NULL

#' An example of a structural variant data frame used by \code{cnv.plot}.
#'
#' @docType data
#' @keywords datasets
#' @name SVExample
NULL

#' A \code{GRanges} instance that contains hg19 gene locations taken from
#' RefSeq with corresponding gene symbols in the \code{mcol} attribute.
#'
#' @docType data
#' @keywords datasets
#' @name hg19Genes
NULL
