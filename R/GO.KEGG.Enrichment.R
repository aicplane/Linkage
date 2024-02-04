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
      gene.position <-
        read.table(
          "inst/extdata/homo.gene_positions.plus.txt",
          header = T,
          sep = "\t"
        )
      gene.position$entrezgene_id <-
        as.character(gene.position$entrezgene_id)
      print(head(gene.position))
      if (genelist_idtype != "entrezgene_id") {
        gene_list <-
          gene.position[gene.position[[genelist_idtype]] %in% gene_list, 3]
      }
      OrgDb <- "org.Hs.eg.db"
      # organism <- 'hsa'
      print(gene_list)
    }

    if (Species == "Mus") {
      gene.position <-
        read.table(
          "inst/extdata/mus.gene_positions.plus.txt",
          header = T,
          sep = "\t"
        )
      if (genelist_idtype != "entrezgene_id") {
        gene_list <-
          gene.position[gene.position[[genelist_idtype]] %in% gene_list, 3]
      }
      OrgDb <- "org.Mm.eg.db"
      # organism <- 'mmu'
      print(gene_list)
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
      gene.position <-
        read.table(
          "inst/extdata/homo.gene_positions.plus.txt",
          header = T,
          sep = "\t"
        )
      gene.position$entrezgene_id <-
        as.character(gene.position$entrezgene_id)
      print(head(gene.position))
      if (genelist_idtype != "entrezgene_id") {
        gene_list <-
          gene.position[gene.position[[genelist_idtype]] %in% gene_list, 3]
      }
      OrgDb <- "org.Hs.eg.db"
      organism <- 'hsa'
      print(gene_list)
    }

    if (Species == "Mus") {
      gene.position <-
        read.table(
          "inst/extdata/mus.gene_positions.plus.txt",
          header = T,
          sep = "\t"
        )
      if (genelist_idtype != "entrezgene_id") {
        gene_list <-
          gene.position[gene.position[[genelist_idtype]] %in% gene_list, 3]
      }
      OrgDb <- "org.Mm.eg.db"
      organism <- 'mmu'
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


dotplot <- function(object, plotly = TRUE, ...) {
  if (plotly == TRUE) {
    p <- plotly::ggplotly(
      enrichplot::dotplot(object) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
        labels = function(x)
          stringr::str_wrap(x, width = 100)
      )
    )
  }
  if (plotly == FALSE) {
    p <-
      enrichplot::dotplot(object) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
        labels = function(x)
          stringr::str_wrap(x, width = 100)
      )
  }
  return(p)
}

upsetplot <- function(object, ...) {
  p <-
    enrichplot::upsetplot(object) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
      labels = function(x)
        stringr::str_wrap(x, width = 100)
    )

  return(p)
}

cnetplot <- function(object, ...) {
  p <-
    enrichplot::cnetplot(object) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
      labels = function(x)
        stringr::str_wrap(x, width = 100)
    )
  return(p)
}

barplot <- function(object, plotly = TRUE, ...) {
  if (plotly == TRUE) {
    p <-   plotly::ggplotly(
      graphics::barplot(a) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
        labels = function(x)
          stringr::str_wrap(x, width = 100)
      )
    )
  }
  if (plotly == FALSE) {
    p <-
      graphics::barplot(a) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
        labels = function(x)
          stringr::str_wrap(x, width = 100)
      )
  }
  return(p)
}


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
      p <- wordcloud::wordcloud(wcdf, size = 1)
    }
  }
  return(p)
}
