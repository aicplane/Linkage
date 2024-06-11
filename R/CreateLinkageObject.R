#' Linkage Class
#'
#' @slot RNA.mtrix RNA-seq count.
#' @slot ATAC.matrix ATAC-seq count.
#' @slot active.gene selected genes.
#' @slot cor.peak Regulatory peaks for each selected genes.
#' @slot cor.peak.annotation Regulatory peaks annotation.
#' @slot detailpeakannotation Regulatory peaks annotation detail.
#' @slot Motif The all TFs of selected genes.
#' @slot Gene_TF The hight correlation TFs of selected genes.
#' @slot geneid The selcted genes id.
#' @slot Summary Summary
#'
#' @return Linkage Class
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

setMethod(
  "show", "LinkageObject",
  function(object) {
    cat("An LinkageObject", "\n")
    cat(nrow(object@RNA.mtrix), "gene", nrow(object@ATAC.matrix), "peak", "\n")
    cat("Active gene:", length(object@cor.peak), "\n")
    cat("Active peak: positive peak", object@Summary$positive_peak, "negetive peak", object@Summary$negetive_peak, "\n")
    cat("Active TF:", object@Summary$TF_num, "\n")
  }
)

#' Create a Linkage Object
#'
#' @param ATAC_count The chromatin accessibility matrix file is a tab-delimited multi-column data matrix as well, which the first three columns represent chromosome name, start coordinate on the chromosome and end coordinate on the chromosome of the peaks respectively; the remaining columns of the chromatin accessibility matrix file represent normalized or raw chromatin accessibility levels of peaks for each sample.
#' @param RNA_count The gene expression matrix file is a tab-delimited multi-column data matrix, which the first column represents gene symbols and the following columns represent normalized or raw expression levels of genes for each sample.
#' @param id_type The RNA_count gene id type. There are ensembl_gene_id, external_gene_name, entrezgene_id types of genetic IDs to choose from.
#' @param Species Select the species, Homo or Mus.
#'
#' @return A Linkage Object
#' @export
#'
#' @importFrom dplyr select
#'
#' @examples
#' library(LinkageR)
#' LinkageObject <-
#'   CreateLinkageObject(
#'     ATAC_count = Small_ATAC,
#'     RNA_count = Small_RNA,
#'     Species = "Homo",
#'     id_type = "ensembl_gene_id"
#'   )
CreateLinkageObject <- function(ATAC_count, RNA_count, Species, id_type) {
  if(Species == "Homo"){
    position <- Homo.position
  }else{
    position <- Mus.position
  }
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
    ATAC_count <- select(ATAC_count,1,3,2,everything())
  }

  LinkageObject <- new("LinkageObject", RNA.mtrix = RNA_count, ATAC.matrix = ATAC_count)
  return(LinkageObject)
}
