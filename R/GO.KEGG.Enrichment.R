#' GO enrichment analysis
#'
#' @param gene_list The list of genes, which is required to be ensembl_gene_id, external_gene_name, entrezgene_id one of the three gene types.
#' @param Species Select the species, Homo or Mus.
#' @param genelist_idtype There are ensembl_gene_id, external_gene_name, entrezgene_id types of genetic IDs to choose from.
#' @param pvalueCutoff Cutoff value of pvalue.
#' @param qvalueCutoff Cutoff value of qvalue.
#' @param minGSSize Minimal size of genes annotated by Ontology term for testing.
#' @param maxGSSize Maximal size of genes annotated for testing.
#'
#' @return A GO object.
#' @export
#'
#' @importFrom clusterProfiler enrichGO
#'
#' @examples
#' library(Linkage)
#' data("SASPGeneSet")
#' go <-
#'   GOEnrichment(gene_list = SASPGeneSet,
#'                Species = "Homo",
#'                genelist_idtype = "external_gene_name")
GOEnrichment <-
  function(gene_list,
           Species = "Homo",
           genelist_idtype,
           pvalueCutoff = 0.05,
           qvalueCutoff = 0.2,
           minGSSize = 10,
           maxGSSize = 500
           ) {
    if (Species == "Homo") {
      gene.position <- Homo.position
      gene.position$entrezgene_id <-
        as.character(gene.position$entrezgene_id)
      if (genelist_idtype != "entrezgene_id") {
        gene_list <-
          gene.position[gene.position[[genelist_idtype]] %in% gene_list, 3]
      }
      OrgDb <- "org.Hs.eg.db"
    }
    if (Species == "Mus") {
      gene.position <- Mus.position
      gene.position$entrezgene_id <-
        as.character(gene.position$entrezgene_id)
      if (genelist_idtype != "entrezgene_id") {
        gene_list <-
          gene.position[gene.position[[genelist_idtype]] %in% gene_list, 3]
      }
      OrgDb <- "org.Mm.eg.db"
    }

    go <- enrichGO(
      gene = gene_list,
      OrgDb = OrgDb,
      ont = "ALL",
      pAdjustMethod = "BH",
      pvalueCutoff = pvalueCutoff,
      qvalueCutoff = qvalueCutoff,
      minGSSize = minGSSize,
      maxGSSize = maxGSSize,
      readable = T
    )
    return(go)
  }


#' KEGG enrichment analysis
#'
#' @param gene_list The list of genes, which is required to be ensembl_gene_id, external_gene_name, entrezgene_id one of the three gene types.
#' @param Species Select the species, Homo or Mus.
#' @param genelist_idtype There are ensembl_gene_id, external_gene_name, entrezgene_id types of genetic IDs to choose from.
#' @param pvalueCutoff Cutoff value of pvalue.
#' @param qvalueCutoff Cutoff value of qvalue.
#' @param minGSSize Minimal size of genes annotated by Ontology term for testing.
#' @param maxGSSize Maximal size of genes annotated for testing.
#'
#' @return A KEGG object.
#' @export
#'
#' @importFrom clusterProfiler enrichKEGG
#'
#' @examples
#' library(Linkage)
#' data("SASPGeneSet")
#' KEGG <-
#'   KEGGEnrichment(
#'     gene_list = SASPGeneSet,
#'     Species = "Homo",
#'     genelist_idtype = "external_gene_name"
#'   )
KEGGEnrichment <-
  function(gene_list,
           Species = "Homo",
           genelist_idtype,
           pvalueCutoff = 0.05,
           qvalueCutoff = 0.2,
           minGSSize = 10,
           maxGSSize = 500
           ) {
    if (Species == "Homo") {
      gene.position <- Homo.position
      gene.position$entrezgene_id <-
        as.character(gene.position$entrezgene_id)
      if (genelist_idtype != "entrezgene_id") {
        gene_list <-
          gene.position[gene.position[[genelist_idtype]] %in% gene_list, 3]
      }
      organism <- "hsa"
    }
    if (Species == "Mus") {
      gene.position <- Mus.position
      gene.position$entrezgene_id <-
        as.character(gene.position$entrezgene_id)
      if (genelist_idtype != "entrezgene_id") {
        gene_list <-
          gene.position[gene.position[[genelist_idtype]] %in% gene_list, 3]
      }
      organism <- "mmu"
    }

    KEGG <- enrichKEGG(
      gene = gene_list,
      organism = organism,
      pvalueCutoff = pvalueCutoff,
      qvalueCutoff = qvalueCutoff,
      minGSSize = minGSSize,
      maxGSSize = maxGSSize,
    )
    return(KEGG)
  }


#' Enrichment analysis Dotplot
#'
#' @param object Go or KEGG object.
#' @param size font size.
#'
#' @return Enrichment dot plot.
#' @export
#'
#' @importFrom enrichplot dotplot
#' @importFrom stringr str_wrap
#' @import ggplot2
#'
#' @examples
#' library(Linkage)
#' data("SASPGeneSet")
#' go <-
#'   GOEnrichment(gene_list = SASPGeneSet,
#'                Species = "Homo",
#'                genelist_idtype = "external_gene_name")
#' p <- EnrichDotPlot(go)
EnrichDotPlot <- function(object,size = 8) {
    p <-
      dotplot(object) + theme(axis.text.y = element_text(size = size)) + scale_y_discrete(
        labels = function(x) {
          str_wrap(x, width = 100)
        }
      )
  return(p)
}


#' Enrichment analysis Upsetplot
#'
#' @param object Go or KEGG object
#' @param size font size.
#'
#' @return Enrichment upset plot.
#' @export
#'
#' @importFrom enrichplot upsetplot
#' @importFrom stringr str_wrap
#' @import ggplot2
#'
#' @examples
#' library(Linkage)
#' data("SASPGeneSet")
#' go <-
#'   GOEnrichment(gene_list = SASPGeneSet,
#'                Species = "Homo",
#'                genelist_idtype = "external_gene_name")
#' p <- EnrichUpsetPlot(go)
EnrichUpsetPlot <- function(object, size = 8) {
  p <-
    upsetplot(object) + theme(axis.text.y = element_text(size = size)) + scale_y_discrete(
      labels = function(x) {
        str_wrap(x, width = 100)
      }
    )

  return(p)
}

#' Enrichment analysis Cnetplot
#'
#' @param object Go or KEGG object
#' @param size font size.
#'
#' @return Enrichment network.
#' @export
#'
#' @importFrom enrichplot cnetplot
#' @importFrom stringr str_wrap
#' @import ggplot2
#'
#' @examples
#' library(Linkage)
#' data("SASPGeneSet")
#' go <-
#'   GOEnrichment(gene_list = SASPGeneSet,
#'                Species = "Homo",
#'                genelist_idtype = "external_gene_name")
#' p <- EnrichCnetPlot(go)
EnrichCnetPlot <- function(object, size = 8) {
  p <-
    cnetplot(object) + theme(axis.text.y = element_text(size)) + scale_y_discrete(
      labels = function(x) {
        str_wrap(x, width = 100)
      }
    )
  return(p)
}

#' Enrichment analysis Barplot
#'
#' @param object Go or KEGG object.
#' @param size font size.
#'
#' @return Enrichment bar plot.
#' @export
#'
#' @importFrom graphics barplot
#' @importFrom stringr str_wrap
#' @import ggplot2
#'
#' @examples
#' library(Linkage)
#' data("SASPGeneSet")
#' go <-
#'   GOEnrichment(gene_list = SASPGeneSet,
#'                Species = "Homo",
#'                genelist_idtype = "external_gene_name")
#' p <- EnrichBarPlot(go)
EnrichBarPlot <- function(object, size = 8) {
    p <-
      barplot(object) + theme(axis.text.y = element_text(size)) + scale_y_discrete(
        labels = function(x) {
          str_wrap(x, width = 100)
        }
      )
  return(p)
}


#' Enrichment analysis Wordcloud
#'
#' @param object Go or KEGG object.
#' @param Type Indicate whether it is a GO or KEGG object, GO and KEGG can be selected.
#' @param plotly logical value,Whether or not to use plotly.The default is TRUE.
#'
#' @return Enrichment wordcloud.
#' @export
#'
#' @importFrom dplyr select
#' @importFrom wordcloud2 wordcloud2
#' @importFrom wordcloud wordcloud
#'
#' @examples
#' library(Linkage)
#' data("SASPGeneSet")
#' go <-
#'   GOEnrichment(gene_list = SASPGeneSet,
#'                Species = "Homo",
#'                genelist_idtype = "external_gene_name")
#' p <- EnrichWordCloud(go, Type = "GO")
EnrichWordCloud <- function(object, Type, plotly = TRUE) {
  if (Type == "GO") {
    wcdf <- read.table(text = object$GeneRatio, sep = "/")[1]
    wcdf$word <- object[, 3]
    names(wcdf)[1] <- "freq"
    wcdf <- select(wcdf, c(2, 1))
    if (plotly == TRUE) {
      p <- wordcloud2(wcdf, size = 1)
    }
    if (plotly == FALSE) {
      p <- wordcloud(wcdf, size = 1)
    }
  }
  if (Type == "KEGG") {
    wcdf <- read.table(text = object$GeneRatio, sep = "/")[1]
    wcdf$word <- object[, 2]
    names(wcdf)[1] <- "freq"
    wcdf <- select(wcdf, c(2, 1))
    if (plotly == TRUE) {
      p <- wordcloud2(wcdf, size = 1)
    }
    if (plotly == FALSE) {
      p <- wordcloud(wcdf$word, wcdf$freq, scale = c(5, 1), colors = RColorBrewer::brewer.pal(8, "Dark2"))
    }
  }
  return(p)
}
