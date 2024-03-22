#' Create Network Object.
#'
#' @param LinkageObject An Linkage Object after MultipleMotifAnalysis.
#' @param genelist_idtype There are ensembl_gene_id, external_gene_name, entrezgene_id types of genetic IDs to choose from.
#'
#' @return Network Object.
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
#' gene_list <- c("TSPAN6", "CD99", "KLHL13", "ARX", "HCCS")
#' LinkageObject <- regulatory_peak(LinkageObject = LinkageObject, gene_list = gene_list, genelist_idtype = "external_gene_name")
#' LinkageObject <- MultipleMotifAnalysis(LinkageObject = LinkageObject,Species = "Homo",TF_cor_method = "pearson")
#' LinkageObject <- CreateNetworkObject(LinkageObject = LinkageObject, genelist_idtype = "entrezgene_id", filter_col = "FDR", filter_value = 0.01)
CreateNetworkObject <- function(LinkageObject, genelist_idtype, filter_col, filter_value) {
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
      Motif = LinkageObject@Motif,
      Gene_TF = Gene.TF.frame.filter,
      geneid = LinkageObject@geneid,
      Summary = list("genelist_idtype" = genelist_idtype,"filter_col" = filter_col)
    )
  return(LinkageObject)
}


#' BuildNetwork.
#'
#' @param LinkageObject An Linkage Object after CreateNetworkObject.
#'
#' @return An visNetwork
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
#' gene_list <- c("TSPAN6", "CD99", "KLHL13", "ARX", "HCCS")
#' LinkageObject <- regulatory_peak(LinkageObject = LinkageObject, gene_list = gene_list, genelist_idtype = "external_gene_name")
#' LinkageObject <- MultipleMotifAnalysis(LinkageObject = LinkageObject,Species = "Homo",TF_cor_method = "pearson")
#' LinkageObject <- CreateNetworkObject(LinkageObject = LinkageObject, genelist_idtype = "entrezgene_id", filter_col = "FDR", filter_value = 0.01)
#' BuildNetwork(LinkageObject)
BuildNetwork <- function(LinkageObject){

  index <- LinkageObject@Summary$genelist_idtype
  TF_filter_method <- LinkageObject@Summary$filter_col

  Gene.TF.frame.filter <- LinkageObject@Gene_TF
  Gene.TF.frame.filter <-
    dplyr::select(
      Gene.TF.frame.filter,
      c("gene", "TF", "p_value", "FDR", "rho")
    ) %>% na.omit()

  negative.TF <-
    Gene.TF.frame.filter[Gene.TF.frame.filter$rho < 0, ]$TF
  positive.TF <-
    Gene.TF.frame.filter[Gene.TF.frame.filter$rho > 0, ]$TF

  Gene <- Gene.TF.frame.filter %>%
    dplyr::distinct(gene) %>%
    dplyr::rename(label = gene)

  # 目的地去重
  TFs <- Gene.TF.frame.filter %>%
    dplyr::distinct(TF) %>%
    dplyr::rename(label = TF)

  ## 合并数据并添加一列索引
  nodes <- rbind(Gene, TFs)
  nodes <- nodes %>%
    dplyr::mutate(id = 1:nrow(nodes)) %>%
    dplyr::select(id, everything())
  nodes <- nodes[!duplicated(nodes$label), ]
  # id has to be the same like from and to columns in edges
  nodes$id <- nodes$label

  edges <- Gene.TF.frame.filter
  colnames(edges)[c(1:2)] <- c("from", "to")

  if (TF_filter_method == "p_value" | TF_filter_method == "FDR") {
    edges$value <- abs(log10(edges[[TF_filter_method]]))
  }
  if (TF_filter_method == "rho") {
    edges$value <- abs(edges[["rho"]])
  }
  # Create graph for Louvain
  graph <- igraph::graph_from_data_frame(edges, directed = FALSE)
  # Louvain Comunity Detection
  cluster <- igraph::cluster_louvain(graph)
  cluster_df <- data.frame(as.list(igraph::membership(cluster)),check.names = F)
  cluster_df <- as.data.frame(t(cluster_df))
  cluster_df$label <- rownames(cluster_df)

  nodes$label <- as.character(nodes$label)
  nodes <- dplyr::left_join(nodes, cluster_df, by = "label")

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
      # print(RNA_count[-which(duplicated(RNA_count[[index]])),])
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

  network <- visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visGroups(groupname = "Gene", color = "red") %>%
    visNetwork::visGroups(groupname = "Negative TF", color = "lightblue") %>%
    visNetwork::visGroups(groupname = "Positive TF", color = "lightblue") %>%
    visNetwork::visIgraphLayout() %>%
    visNetwork::visEdges(arrows = "from") %>%
    visNetwork::visInteraction(
      navigationButtons = F,
      dragNodes = T,
      dragView = T,
      zoomView = T
    ) %>%
    visNetwork::visOptions(
      highlightNearest = T,
      nodesIdSelection = TRUE,
      selectedBy = "group"
    )
  return(network)
}


