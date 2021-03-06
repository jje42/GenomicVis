% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/read.vcf.R
\name{read.vcf}
\alias{read.vcf}
\title{Read VCF}
\usage{
read.vcf(f, genome, exclude.filtered = TRUE, read.info = FALSE,
  read.geno = FALSE, filter.func = NULL)
}
\arguments{
\item{f}{A \code{TabixFile} instance or character() name of the VCF file to
be processed.}

\item{genome}{A genome identifier.}

\item{exclude.filtered}{filt}

\item{read.info}{Read INFO field, default is to ignore the INFO field.  Set
this to \code{TRUE} if INFO fields are required in the filter function (see
Details).}

\item{read.geno}{Read genotypes from VCF, the default is to ignore the
genotypes. Set this to \code{TRUE} if genotypes are needed in the filter
function (see Details).}

\item{filter.func}{a function used to filter variants from the returned
\code{GRanges}}
}
\value{
A \code{VCF} instance from the \pkg{VariantAnnotation} package.
}
\description{
Read a VCF file and return the called SNVs as a \code{GRanges} instance.
Only SNVs with a single ALT allele are returned, and, by default, all
filtered SNVs are excluded from the returned value.
}
\details{
details
}
\examples{
\dontrun{
# To simply read a VCF excluding filtered variants use
x <- read.vcf(f, 'hg19', read.info = TRUE, filter.func = func)

# For more complicated filtering you can pass a filter function.  This
# example assuming the VCF has been annotated with SnpEff and we are only
# interested in variants that have not been filtered and have a SnpEff
# impact of HIGH or MODERATE.

func <- function(vcf) {
  x <- rowData(vcf)
  x[unlist(lapply(info(vcf)$EFF,
    function(x) any(grepl('(HIGH)|(MODERATE)', x))))]
}

f <- system.file('extdata', 'example.vcf', package = 'GenomicVis')
x <- read.vcf(f, 'hg19', read.info = TRUE, filter.func = func)
}
}

