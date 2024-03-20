#' GO enrichment analysis
#'
#' @param gene_list The list of genes, which is required to be ensembl_gene_id, external_gene_name, entrezgene_id one of the three gene types.
#' @param Species Select the species, Homo or Mus.
#' @param genelist_idtype There are ensembl_gene_id, external_gene_name, entrezgene_id types of genetic IDs to choose from.
#' @param pvalueCutoff Cutoff value of pvalue.
#' @param qvalueCutoff Cutoff value of qvalue.
#' @param minGSSize Minimal size of genes annotated by Ontology term for testing.
#' @param maxGSSize Maximal size of genes annotated for testing.
#' @param ... ...
#'
#' @return A GO object.
#' @export
#'
#' @examples
#' extdatadir <- system.file("extdata", "Senescence-associated secretory phenotype.txt", package = "Linkage")
#' gene_list <- read.table(extdatadir)
#' go <- GO.enrichment(gene_list = gene_list$V1, Species = "Homo", genelist_idtype = "external_gene_name")
GO.enrichment <-
  function(gene_list,
           Species = "Homo",
           genelist_idtype,
           pvalueCutoff = 0.05,
           qvalueCutoff = 0.2,
           minGSSize = 10,
           maxGSSize = 500,
           ...) {
    if (Species == "Homo") {
      dir <- system.file("extdata", "homo.gene_positions.plus.txt", package = "Linkage")
      gene.position <- read.table(dir,header = TRUE,sep = "\t",check.names = F)
      gene.position$entrezgene_id <-
        as.character(gene.position$entrezgene_id)
      # print(head(gene.position))
      if (genelist_idtype != "entrezgene_id") {
        gene_list <-
          gene.position[gene.position[[genelist_idtype]] %in% gene_list, 3]
      }
      OrgDb <- "org.Hs.eg.db"
      # organism <- 'hsa'
      # print(gene_list)
    }

    if (Species == "Mus") {
      dir <- system.file("extdata", "mus.gene_positions.plus.txt", package = "Linkage")
      gene.position <- read.table(dir,header = TRUE,sep = "\t",check.names = F)
      gene.position$entrezgene_id <-
        as.character(gene.position$entrezgene_id)
      if (genelist_idtype != "entrezgene_id") {
        gene_list <-
          gene.position[gene.position[[genelist_idtype]] %in% gene_list, 3]
      }
      OrgDb <- "org.Mm.eg.db"
      # organism <- 'mmu'
      # print(gene_list)
    }

    go <- clusterProfiler::enrichGO(
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
#' @param ... ...
#'
#' @return A KEGG object.
#' @export
#'
#' @examples
#' extdatadir <- system.file("extdata", "Senescence-associated secretory phenotype.txt", package = "Linkage")
#' gene_list <- read.table(extdatadir)
#' KEGG <- KEGG.enrichment(gene_list = gene_list$V1, Species = "Homo", genelist_idtype = "external_gene_name")
KEGG.enrichment <-
  function(gene_list,
           Species = "Homo",
           genelist_idtype,
           pvalueCutoff = 0.05,
           qvalueCutoff = 0.2,
           minGSSize = 10,
           maxGSSize = 500,
           ...) {
    if (Species == "Homo") {
      dir <- system.file("extdata", "homo.gene_positions.plus.txt", package = "Linkage")
      gene.position <- read.table(dir,header = TRUE,sep = "\t",check.names = F)
      gene.position$entrezgene_id <-
        as.character(gene.position$entrezgene_id)
      print(head(gene.position))
      if (genelist_idtype != "entrezgene_id") {
        gene_list <-
          gene.position[gene.position[[genelist_idtype]] %in% gene_list, 3]
      }
      # OrgDb <- "org.Hs.eg.db"
      organism <- "hsa"
      print(gene_list)
    }

    if (Species == "Mus") {
      dir <- system.file("extdata", "mus.gene_positions.plus.txt", package = "Linkage")
      gene.position <- read.table(dir,header = TRUE,sep = "\t",check.names = F)
      gene.position$entrezgene_id <-
        as.character(gene.position$entrezgene_id)
      if (genelist_idtype != "entrezgene_id") {
        gene_list <-
          gene.position[gene.position[[genelist_idtype]] %in% gene_list, 3]
      }
      # OrgDb <- "org.Mm.eg.db"
      organism <- "mmu"
      print(gene_list)
    }

    KEGG <- clusterProfiler::enrichKEGG(
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
#' @param plotly logical value,Whether or not to use plotly.The default is TRUE.
#' @param ... ...
#'
#' @return Enrichment dot plot.
#' @export
#'
#' @examples
#' extdatadir <- system.file("extdata", "Senescence-associated secretory phenotype.txt", package = "Linkage")
#' gene_list <- read.table(extdatadir)
#' go <- GO.enrichment(gene_list = gene_list$V1, Species = "Homo", genelist_idtype = "external_gene_name")
#' dotplot(go)
dotplot <- function(object, plotly = FALSE, ...) {
  if (plotly == TRUE) {
    p <- plotly::ggplotly(
      enrichplot::dotplot(object) + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8)) + ggplot2::scale_y_discrete(
        labels = function(x) {
          stringr::str_wrap(x, width = 100)
        }
      ),
      dynamicTicks = TRUE
    )
  }
  if (plotly == FALSE) {
    p <-
      enrichplot::dotplot(object) + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8)) + ggplot2::scale_y_discrete(
        labels = function(x) {
          stringr::str_wrap(x, width = 100)
        }
      )
  }
  return(p)
}


#' Enrichment analysis Upsetplot
#'
#' @param object Go or KEGG object
#' @param ... ...
#'
#' @return Enrichment upset plot.
#' @export
#'
#' @examples
#' extdatadir <- system.file("extdata", "Senescence-associated secretory phenotype.txt", package = "Linkage")
#' gene_list <- read.table(extdatadir)
#' go <- GO.enrichment(gene_list = gene_list$V1, Species = "Homo", genelist_idtype = "external_gene_name")
#' upsetplot(go)
upsetplot <- function(object, ...) {
  p <-
    enrichplot::upsetplot(object) + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8)) + ggplot2::scale_y_discrete(
      labels = function(x) {
        stringr::str_wrap(x, width = 100)
      }
    )

  return(p)
}

#' Enrichment analysis Cnetplot
#'
#' @param object Go or KEGG object
#' @param ... ...
#'
#' @return Enrichment network.
#' @export
#'
#' @examples
#' extdatadir <- system.file("extdata", "Senescence-associated secretory phenotype.txt", package = "Linkage")
#' gene_list <- read.table(extdatadir)
#' go <- GO.enrichment(gene_list = gene_list$V1, Species = "Homo", genelist_idtype = "external_gene_name")
#' cnetplot(go)
cnetplot <- function(object, ...) {
  p <-
    enrichplot::cnetplot(object) + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8)) + ggplot2::scale_y_discrete(
      labels = function(x) {
        stringr::str_wrap(x, width = 100)
      }
    )
  return(p)
}

#' Enrichment analysis Barplot
#'
#' @param object Go or KEGG object.
#' @param plotly logical value,Whether or not to use plotly.The default is TRUE.
#' @param ... ...
#'
#' @return Enrichment bar plot.
#' @export
#'
#' @examples
#' extdatadir <- system.file("extdata", "Senescence-associated secretory phenotype.txt", package = "Linkage")
#' gene_list <- read.table(extdatadir)
#' go <- GO.enrichment(gene_list = gene_list$V1, Species = "Homo", genelist_idtype = "external_gene_name")
#' barplot(go)
barplot <- function(object, plotly = FALSE, ...) {
  if (plotly == TRUE) {
    p <- plotly::ggplotly(
      graphics::barplot(object) + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8)) + ggplot2::scale_y_discrete(
        labels = function(x) {
          stringr::str_wrap(x, width = 100)
        }
      )
    )
  }
  if (plotly == FALSE) {
    p <-
      graphics::barplot(object) + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8)) + ggplot2::scale_y_discrete(
        labels = function(x) {
          stringr::str_wrap(x, width = 100)
        }
      )
  }
  return(p)
}


#' Enrichment analysis Wordcloud
#'
#' @param object Go or KEGG object.
#' @param Type Indicate whether it is a GO or KEGG object, GO and KEGG can be selected.
#' @param plotly logical value,Whether or not to use plotly.The default is TRUE.
#' @param ... ...
#'
#' @return Enrichment wordcloud.
#' @export
#'
#' @examples
#' extdatadir <- system.file("extdata", "Senescence-associated secretory phenotype.txt", package = "Linkage")
#' gene_list <- read.table(extdatadir)
#' go <- GO.enrichment(gene_list = gene_list$V1, Species = "Homo", genelist_idtype = "external_gene_name")
#' wordcloud(go, Type = "GO")
wordcloud <- function(object, Type, plotly = TRUE, ...) {
  if (Type == "GO") {
    wcdf <- read.table(text = object$GeneRatio, sep = "/")[1]
    wcdf$word <- object[, 3]
    names(wcdf)[1] <- "freq"
    wcdf <- dplyr::select(wcdf, c(2, 1))
    if (plotly == TRUE) {
      p <- wordcloud2::wordcloud2(wcdf, size = 1)
    }
    if (plotly == FALSE) {
      p <- wordcloud::wordcloud(wcdf, size = 1)
    }
  }
  if (Type == "KEGG") {
    wcdf <- read.table(text = object$GeneRatio, sep = "/")[1]
    wcdf$word <- object[, 2]
    names(wcdf)[1] <- "freq"
    wcdf <- dplyr::select(wcdf, c(2, 1))
    if (plotly == TRUE) {
      p <- wordcloud2::wordcloud2(wcdf, size = 1)
    }
    if (plotly == FALSE) {
      p <- wordcloud::wordcloud(wcdf$word, wcdf$freq, scale = c(5, 1), colors = RColorBrewer::brewer.pal(8, "Dark2"))
    }
  }
  return(p)
}
