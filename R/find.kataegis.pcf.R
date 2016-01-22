#' @import parallel
#' @import IRanges
#' @import GenomicRanges
#' @import GenomeInfoDb
find.kataegis.pcf <- function(x, min.snvs, max.mean.distance, maxseg = NA, 
    maxk = NA, ncpus = getOption('cl.cores', 1)) {
  # x = GRanges
  chrs <- as.character(runValue(seqnames(x)))

  if (ncpus > 1) {
    cl <- makeCluster(ncpus)
    # clusterEvalQ(cl, { library('GenomicRanges'); library(tilingArray) })
    clusterExport(cl, 'get.segments', envir = environment())
    clusterExport(cl, 'reduceWithin', envir = environment())
    gr <- tryCatch({
      parSapplyLB(cl = cl, chrs, find.kataegis.pcf.helper, x, min.snvs, 
	max.mean.distance, maxseg, maxk)
    },
    finally = {
      stopCluster(cl)
    })
    unlist(GRangesList(Filter(Negate(is.null), gr)))
  } else {
    gr <- sapply(chrs, find.kataegis.pcf.helper, x, min.snvs, 
      max.mean.distance, maxseg, maxk)
    unlist(GRangesList(Filter(Negate(is.null), gr))) 
  }
}
