#' The LinkageObject class
#'
#' @slot RNA.mtrix data.frame.
#' @slot ATAC.matrix data.frame.
#' @slot active.gene data.frame.
#' @slot cor.peak list.
#' @slot cor.peak.annotation list.
#' @slot Motif list.
#' @slot Gene_TF data.frame.
#' @slot geneid character.
#' @slot NetworkSetting list.
#'
#' @return Linkage Object
#' @export
setClass("LinkageObject",
         slots = list(
           RNA.mtrix = "data.frame",
           ATAC.matrix = "data.frame",
           active.gene = "data.frame",
           cor.peak = "list",
           cor.peak.annotation = "list",
           detailpeakannotation = "list",
           Motif = "list",
           Gene_TF = "data.frame",
           geneid = "character",
           Summary = "list"
         ),
         prototype = list(
           RNA.mtrix = data.frame(),
           ATAC.matrix = data.frame(),
           active.gene = data.frame(),
           cor.peak = list(),
           cor.peak.annotation = list(),
           detailpeakannotation = list(),
           Motif = list(),
           Gene_TF = data.frame(),
           geneid = character(),
           Summary = list(
             "positive_peak" = 0,
             "negetive_peak" = 0,
             "TF_num" = 0,
             "genelist_idtype" = NULL,
             "filter_col" = NULL
           )
         )
)
#' show method for \code{LinkageObject} instance
#'
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,LinkageObject,ANY-method
#' @title show method
#' @param object A \code{LinkageObject} instance
#' @return message
#' @importFrom methods show
#' @exportMethod show
#' @usage show(object)
setMethod(
  "show", "LinkageObject",
  function(object) {
    cat("An LinkageObject", "\n")
    cat(nrow(LinkageObject@RNA.mtrix), "gene", nrow(LinkageObject@ATAC.matrix), "peak", "\n")
    cat("Active gene:", length(LinkageObject@cor.peak), "\n")
    cat("Active peak: positive peak", LinkageObject@Summary$positive_peak, "negetive peak", LinkageObject@Summary$negetive_peak, "\n")
    cat("Active TF:", LinkageObject@Summary$TF_num, "\n")
  }
)

#' Create a Linkage Object
#'
#' @param ATAC_count The chromatin accessibility matrix file is a tab-delimited multi-column data matrix as well, which the first three columns represent chromosome name, start coordinate on the chromosome and end coordinate on the chromosome of the peaks respectively; the remaining columns of the chromatin accessibility matrix file represent normalized or raw chromatin accessibility levels of peaks for each sample.
#' @param RNA_count The gene expression matrix file is a tab-delimited multi-column data matrix, which the first column represents gene symbols and the following columns represent normalized or raw expression levels of genes for each sample.
#' @param id_type The RNA_count gene id type. There are ensembl_gene_id, external_gene_name, entrezgene_id types of genetic IDs to choose from.
#'
#' @return A Linkage Object
#' @export
#'
#' @examples
#' RNA.seq.dir <- system.file("extdata", "TCGA-BRCA-RNA.txt", package = "Linkage")
#' RNA.seq <- data.table::fread(RNA.seq.dir, header = TRUE)
#' Homo.list.files.dir <- system.file("extdata", "Homo.ATAC", package = "Linkage")
#' Homo.list.files <- list.files(Homo.list.files.dir)
#' Homo.list.files <- paste0(Homo.list.files.dir, "/", Homo.list.files)
#' Homo.df_list <- lapply(Homo.list.files, function(file) data.table::fread(file, header = TRUE))
#' ATAC.seq <- do.call(rbind, Homo.df_list)
#' LinkageObject <- CreateLinkageObject(ATAC_count = ATAC.seq, RNA_count = RNA.seq, Species = "Homo", id_type = "ensembl_gene_id")
CreateLinkageObject <- function(ATAC_count, RNA_count, Species, id_type) {
  if(Species == "Homo"){
    position.dir <- system.file("extdata", "homo.gene_positions.plus.txt", package = "Linkage")
  }else{
    position.dir <- system.file("extdata", "mus.gene_positions.plus.txt", package = "Linkage")
  }
  position <- data.table::fread(position.dir, header = TRUE, sep = "\t")
  RNA_count <- merge(position, RNA_count, by.x = id_type, by.y = colnames(RNA_count)[1])

  if (ATAC_count[1, 2] < ATAC_count[1, 3]) {
    names(ATAC_count)[1] <- "chrom"
    names(ATAC_count)[2] <- "chromStart"
    names(ATAC_count)[3] <- "chromEnd"
  }
  if (ATAC_count[1, 2] > ATAC_count[1, 3]) {
    names(ATAC_count)[1] <- "chrom"
    names(ATAC_count)[2] <- "chromEnd"
    names(ATAC_count)[3] <- "chromStart"
    ATAC_count <- dplyr::select(ATAC_count,1,3,2,everything())
  }

  LinkageObject <- new("LinkageObject", RNA.mtrix = RNA_count, ATAC.matrix = ATAC_count)
  return(LinkageObject)
}

# RNA.seq.dir <- system.file("extdata", "TCGA-BRCA-RNA.txt", package = "Linkage")
# RNA.seq <- data.table::fread(RNA.seq.dir, header = TRUE)
# Homo.list.files.dir <- system.file("extdata", "Homo.ATAC", package = "Linkage")
# Homo.list.files <- list.files(Homo.list.files.dir)
# Homo.list.files <- paste0(Homo.list.files.dir, "/", Homo.list.files)
# Homo.df_list <- lapply(Homo.list.files, function(file) data.table::fread(file, header = TRUE))
# ATAC.seq <- do.call(rbind, Homo.df_list)
# LinkageObject <- CreateLinkageObject(ATAC_count = ATAC.seq, RNA_count = RNA.seq, Species = "Homo", id_type = "ensembl_gene_id")
#
# ATAC.seq <- data.table::fread("D:/R/R-4.3.0/library/Linkage/extdata/Homo.ATAC/chrX.txt", header = TRUE)
# RNA.seq <- data.table::fread(RNA.seq.dir, header = TRUE)
# RNA.seq <- LinkageObject@RNA.mtrix[LinkageObject@RNA.mtrix$external_gene_name %in% c("TSPAN6", "CD99", "KLHL13", "ARX", "HCCS"),c(1,7:ncol(LinkageObject@RNA.mtrix))]
# LinkageObject <- CreateLinkageObject(ATAC_count = ATAC.seq, RNA_count = RNA.seq, Species = "Homo", id_type = "ensembl_gene_id")
#
# save(LinkageObject,file = "data/LinkageObject.rdata")
