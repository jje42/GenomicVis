#' List VCF files in a directory.
#'
#' This function produces a character vector of the names (or paths) of VCF
#' files in the named directory.
#'
#' @param path A character vector of full path names; the default corresponds
#' to the working directory, \code{getwd()}.  Tilde expansion (see
#' \code{path.expand}) is performed.
#' @param full.names a logical value. If \code{TRUE}, the directory path is
#' prepended to the file names to give a relative file path.  If \code{FALSE},
#' the file names (rather than paths) are returned.
#' @param recursive logical.  Should the listing recurse into directories?
#' @return A character vector containing the names of the VCF files in the
#' specified directories, or \code{character(0)} if there were no VCF files.
#' @author Jonathan Ellis <jonathan.j.ellis@@gmail.com>
#' @export
list.vcffiles <- function(path = '.', full.names = FALSE, recursive = FALSE) {
  list.files(path, pattern = '\\.vcf$', full.names = full.names, 
    recursive = recursive)
}
