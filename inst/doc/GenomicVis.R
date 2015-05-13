## ----style-knitr, eval=TRUE, echo=FALSE, results="asis"---------------------------------
BiocStyle::latex()

## ---------------------------------------------------------------------------------------
library(GenomicVis)

## ---------------------------------------------------------------------------------------
# We need to import VariantAnnotation to get access to rowData
suppressMessages(library(VariantAnnotation))
vcf.file <- system.file('extdata', 'example.vcf', package = 'GenomicVis')
vcf <- read.vcf(vcf.file, 'GRCh37')
x <- rowData(vcf)
plotKataegis(x)

## ----eval=FALSE-------------------------------------------------------------------------
#  vcf.files <- list.vcffiles()
#  for (vcf.file in vcf.files) {
#    vcf <- read.vcf(vcf.file, 'GRCh37')
#    x <- rowData(vcf)
#    kat <- kataegis(x)
#    name <- tools::file_path_sans_ext(vcf.file)
#    png(file = paste0(name, '.png'))
#    plotKataegis(x, main = name)
#    dev.off()
#  }

## ----eval=FALSE-------------------------------------------------------------------------
#  kat <- kataegis(x, pcf = TRUE, ncpus = 4)

## ----eval=FALSE-------------------------------------------------------------------------
#  kat <- kataegis(x, ncpus = 4)

## ---------------------------------------------------------------------------------------
file.names <- sprintf('LC%s_TUMOUR_%s.vcf', rep(1:3, each = 2), 
  rep(c('A', 'B'), each = 3))
vcf.files <- system.file('extdata', file.names, package = 'GenomicVis')  
sample.names <- tools::file_path_sans_ext(basename(vcf.files))
snv.clustering(vcf.files, sample.names, genome = 'hg19')

## ---------------------------------------------------------------------------------------
file.names <- c('LC1_A.snpeff.vcf', 'LC1_B.snpeff.vcf', 
  'LC1_C.snpeff.vcf', 'LC1_D.snpeff.vcf')
vcf.files <- system.file('extdata', file.names, package = 'GenomicVis')
sample.names <- c('LC1_A', 'LC1_B', 'LC1_C', 'LC1_D')
dat <- read.snpeff.vcfs(vcf.files, 'GRCh37', sample.names)
snv.heatmap(dat, margins = c(5, 9), y.cex.axis = 0.7)

## ----eval=FALSE-------------------------------------------------------------------------
#  library(Vennerable)
#  file.names <- c('LC1_TUMOUR_A.vcf', 'LC1_TUMOUR_B.vcf')
#  vcf.files <- system.file('extdata', file.names, package = 'GenomicVis')
#  sample.names <- c('LC1_A', 'LC1_B')
#  v <- vcf.venn(vcf.files, 'GRCh37', sample.names)
#  plot(v$venn)

## ----eval=TRUE, fig.keep='last'---------------------------------------------------------
data(SNPExample)
data(CNVExample)
data(SVExample)
cnv.plot('18', SNPExample, CNVExample, SVExample)

## ----eval=FALSE-------------------------------------------------------------------------
#  library(TxDb.Hsapiens.UCSC.hg19.knownGene)
#  library(org.Hs.eg.db)
#  genes.gr <- genes(TxDb.Hsapiens.UCSC.hg19.knownGene)
#  gene_ids <- unlist(genes.gr$gene_id)
#  symbol.map <- select(org.Hs.eg.db, gene_ids, 'SYMBOL')
#  genes.gr$symbol <- symbol.map$SYMBOL

## ---------------------------------------------------------------------------------------
data(hg19Genes)
data(CNVData)
set.seed(100)
g <- sample(hg19Genes$symbol, 20)
cnv.heatmap(CNVData, symbols = g, genes.gr = hg19Genes)

## ----eval=FALSE-------------------------------------------------------------------------
#  cnv.heatmap(CNVData, samples = c('LC3_A', 'LC3_B'), genes.gr = hg19Genes)

## ----eval=FALSE-------------------------------------------------------------------------
#  library(org.Hs.eg.db)
#  library(GenomicRanges)
#  library(plyr)
#  
#  x <- read.delim(
#    'hg19_refGene.bed.gz',
#    header = FALSE,
#    stringsAsFactors = FALSE
#  )
#  x <- x[x$V1 %in% paste0("chr", c(1:22, 'X', 'Y')), ]
#  keys <- x$V4
#  dict <- select(org.Hs.eg.db, keys, 'SYMBOL', keytype = 'REFSEQ')
#  symbol.df <- ddply(dict, .(REFSEQ), summarise,
#    symbol = paste(unique(SYMBOL), collapse = ';'))
#  rownames(symbol.df) <- symbol.df$REFSEQ
#  x$symbol <- symbol.df[x$V4, ]$symbol
#  hg19Genes <- GRanges(
#    seqnames = Rle(x$V1),
#    ranges = IRanges(start = x$V2, end = x$V3),
#    strand = Rle(x$V6),
#    refseq = x$V4,
#    symbol = x$symbol
#  )

## ----eval=FALSE-------------------------------------------------------------------------
#  CNVExample <- read.gap(
#    system.file('extdata', 'CN_BA_Illumina_MySeries.chr18.txt',
#      package = 'GenomicVis'),
#    system.file('extdata', 'Illum660K_annot_cut.chr18.csv',
#      package = 'GenomicVis'),
#    'LC3_TUMOUR_C_FinalReport'
#  )

## ----eval=FALSE-------------------------------------------------------------------------
#  filename <- system.file('extdata', 'LC3_TUMOUR_C_FinalReport.chr18.txt',
#    package = 'GenomicVis')
#  SNPExample <- read.illumina(filename)

## ----eval=FALSE-------------------------------------------------------------------------
#  SVExample <- read.breakdancer(
#    system.file('extdata', 'LC3_BLOOD_TUMOUR_C.chr18.txt',
#      package = 'GenomicVis'),
#    normal.regex = 'BLOOD'
#  )
#  SVExample <- filter.breakdancer(SVExample)

## ----eval=FALSE-------------------------------------------------------------------------
#  gap <- read.gap()
#  CNVData <- gap2cnv(gap)
#  CNVData <- CNVData[, c('Chr', 'Begin', 'End', 'LC3_TUMOUR_A_FinalReport',
#    'LC3_TUMOUR_B_FinalReport', 'LC3_TUMOUR_C_FinalReport')]
#  colnames(CNVData) <- sub('_TUMOUR', '', colnames(CNVData))
#  colnames(CNVData) <- sub('_FinalReport', '', colnames(CNVData))

## ----echo=FALSE-------------------------------------------------------------------------
sessionInfo()

