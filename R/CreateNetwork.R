#' Create Network Object.
#'
#' @param LinkageObject An Linkage Object after BuildGRNs.
#' @param genelist_idtype There are ensembl_gene_id, external_gene_name, entrezgene_id types of genetic IDs to choose from.
#' @param filter_col p_value,FDR,rho can be selected.
#' @param filter_value Cutoff value of filter_col.
#'
#' @return Network Object.
#' @export
#'
#' @examples
#' library(LinkageR)
#' data("Small_ATAC.rda")
#' data("Small_RNA.rda")
#' LinkageObject <-
#'   CreateLinkageObject(
#'     ATAC_count = Small_ATAC,
#'     RNA_count = Small_RNA,
#'     Species = "Homo",
#'     id_type = "ensembl_gene_id"
#'   )
#' gene_list <- c("TSPAN6", "CD99", "KLHL13")
#' LinkageObject <-
#'   RegulatoryPeak(
#'     LinkageObject = LinkageObject,
#'     gene_list = gene_list,
#'     genelist_idtype = "external_gene_name"
#'   )
#' LinkageObject <-
#'   BuildGRNs(LinkageObject = LinkageObject,
#'                         Species = "Homo",
#'                         TF_cor_method = "pearson")
#' LinkageObject <-
#'   FilterGRNs(
#'     LinkageObject = LinkageObject,
#'     genelist_idtype = "entrezgene_id",
#'     filter_col = "FDR",
#'     filter_value = 0.01
#'   )
FilterGRNs <- function(LinkageObject, genelist_idtype, filter_col, filter_value) {
  RNA.seq <- LinkageObject@active.gene
  tf <- LinkageObject@Motif
  Gene.TF <- list()
  Gene.TF.frame <- data.frame()
  index <- genelist_idtype
  for (i in 1:nrow(RNA.seq)) {
    if (nrow(tf[[i]]) == 0) {
      Gene.TF[[i]] <- data.frame(
        gene = RNA.seq[i, index],
        TF = NA,
        p_value = NA,
        FDR = NA,
        rho = NA
      )
    } else {
      Gene.TF[[i]] <- data.frame(
        gene = RNA.seq[i, index],
        TF = tf[[i]][, index],
        p_value = tf[[i]]$p_value,
        FDR = tf[[i]]$FDR,
        rho = tf[[i]]$rho
      )
    }
    Gene.TF.frame <- rbind(Gene.TF.frame, Gene.TF[[i]])
  }
  if (filter_col == "p_value") {
    Gene.TF.frame.filter <-
      Gene.TF.frame[(
        rowSums(is.na(Gene.TF.frame)) == 0 &
          Gene.TF.frame$p_value <= filter_value &
          Gene.TF.frame$rho != 1
      ) |
        (rowSums(is.na(Gene.TF.frame)) == 4), ]
  }
  if (filter_col == "FDR") {
    Gene.TF.frame.filter <-
      Gene.TF.frame[(
        rowSums(is.na(Gene.TF.frame)) == 0 &
          Gene.TF.frame$FDR <= filter_value &
          Gene.TF.frame$rho != 1
      ) |
        (rowSums(is.na(Gene.TF.frame)) == 4), ]
  }
  if (filter_col == "rho") {
    Gene.TF.frame.filter <-
      Gene.TF.frame[(
        rowSums(is.na(Gene.TF.frame)) == 0 &
          Gene.TF.frame$rho >= filter_value &
          Gene.TF.frame$rho != 1
      ) |
        (rowSums(is.na(Gene.TF.frame)) == 4), ]
  }
  LinkageObject <-
    new(
      "LinkageObject",
      RNA.mtrix = LinkageObject@RNA.mtrix,
      ATAC.matrix = LinkageObject@ATAC.matrix,
      active.gene = LinkageObject@active.gene,
      cor.peak = LinkageObject@cor.peak,
      cor.peak.annotation = LinkageObject@cor.peak.annotation,
      detailpeakannotation = LinkageObject@detailpeakannotation,
      Motif = LinkageObject@Motif,
      Gene_TF = Gene.TF.frame.filter,
      geneid = LinkageObject@geneid,
      Summary = list(
        "positive_peak" = LinkageObject@Summary$positive_peak,
        "negetive_peak" = LinkageObject@Summary$negetive_peak,
        "TF_num" = nrow(Gene.TF.frame.filter),
        "genelist_idtype" = genelist_idtype,
        "filter_col" = filter_col
      )
    )
  return(LinkageObject)
}


#' VisualGRNs.
#'
#' @param LinkageObject An Linkage Object after FilterGRNs .
#'
#' @return An visNetwork
#' @export
#'
#' @importFrom dplyr select distinct rename mutate left_join
#' @importFrom igraph graph_from_data_frame cluster_louvain membership
#' @import visNetwork
#'
#' @examples
#' library(LinkageR)
#' data("Small_ATAC.rda")
#' data("Small_RNA.rda")
#' LinkageObject <-
#'   CreateLinkageObject(
#'     ATAC_count = Small_ATAC,
#'     RNA_count = Small_RNA,
#'     Species = "Homo",
#'     id_type = "ensembl_gene_id"
#'   )
#' gene_list <- c("TSPAN6", "CD99", "KLHL13")
#' LinkageObject <-
#'   RegulatoryPeak(
#'     LinkageObject = LinkageObject,
#'     gene_list = gene_list,
#'     genelist_idtype = "external_gene_name"
#'   )
#' LinkageObject <-
#'   BuildGRNs(LinkageObject = LinkageObject,
#'                         Species = "Homo",
#'                         TF_cor_method = "pearson")
#' LinkageObject <-
#'   FilterGRNs(
#'     LinkageObject = LinkageObject,
#'     genelist_idtype = "entrezgene_id",
#'     filter_col = "FDR",
#'     filter_value = 0.01
#'   )
#' VisualGRNs(LinkageObject)
VisualGRNs <- function(LinkageObject){

  index <- LinkageObject@Summary$genelist_idtype
  TF_filter_method <- LinkageObject@Summary$filter_col

  Gene.TF.frame.filter <- LinkageObject@Gene_TF
  Gene.TF.frame.filter <-
    select(
      Gene.TF.frame.filter,
      c("gene", "TF", "p_value", "FDR", "rho")
    ) %>% na.omit()

  negative.TF <-
    Gene.TF.frame.filter[Gene.TF.frame.filter$rho < 0, ]$TF
  positive.TF <-
    Gene.TF.frame.filter[Gene.TF.frame.filter$rho > 0, ]$TF

  Gene <- Gene.TF.frame.filter %>%
    distinct(gene) %>%
    rename(label = gene)

  TFs <- Gene.TF.frame.filter %>%
    distinct(TF) %>%
    rename(label = TF)

  nodes <- rbind(Gene, TFs)
  nodes <- nodes %>%
    mutate(id = 1:nrow(nodes)) %>%
    select(id, everything())
  nodes <- nodes[!duplicated(nodes$label), ]
  nodes$id <- nodes$label

  edges <- Gene.TF.frame.filter
  colnames(edges)[c(1:2)] <- c("from", "to")

  if (TF_filter_method == "p_value" | TF_filter_method == "FDR") {
    edges$value <- abs(log10(edges[[TF_filter_method]]))
  }
  if (TF_filter_method == "rho") {
    edges$value <- abs(edges[["rho"]])
  }
  graph <- graph_from_data_frame(edges, directed = FALSE)
  cluster <- cluster_louvain(graph)
  cluster_df <- data.frame(as.list(membership(cluster)),check.names = F)
  cluster_df <- as.data.frame(t(cluster_df))
  cluster_df$label <- rownames(cluster_df)

  nodes$label <- as.character(nodes$label)
  nodes <- left_join(nodes, cluster_df, by = "label")

  colnames(nodes)[3] <- "group"
  nodes$group <-
    ifelse(
      nodes$id %in% Gene.TF.frame.filter$gene,
      "Gene",
      ifelse(nodes$id %in% negative.TF, "Negative TF", "Positive TF")
    )
  nodes$color <-
    ifelse(nodes$id %in% Gene.TF.frame.filter$gene,
           "red",
           "lightblue"
    )
  tryCatch(
    {
      if (length(which(rowSums(is.na(nodes)) > 0)) > 0) {
        nodes <- nodes[-which(rowSums(is.na(nodes)) > 0), ]
      }
      RNA_count <- a@RNA.mtrix
      RNA_count <-
        RNA_count[-which(duplicated(RNA_count[[index]])), ]
      nodes$value <-
        apply(RNA_count[RNA_count[[index]] %in% nodes$label, c(-1:-6)], 1, mean)
    },
    error = function(e) {
      # print(e)
      nodes$value <- 1
    }
  )

  edges <- na.omit(edges)

  edges$color <-
    ifelse(edges$to %in% positive.TF, "#FF8C00", "lightgreen")

  network <- visNetwork(nodes, edges) %>%
    visGroups(groupname = "Gene", color = "red") %>%
    visGroups(groupname = "Negative TF", color = "lightblue") %>%
    visGroups(groupname = "Positive TF", color = "lightblue") %>%
    visIgraphLayout() %>%
    visEdges(arrows = "from") %>%
    visInteraction(
      navigationButtons = F,
      dragNodes = T,
      dragView = T,
      zoomView = T
    ) %>%
    visOptions(
      highlightNearest = T,
      nodesIdSelection = TRUE,
      selectedBy = "group"
    )
  return(network)
}


