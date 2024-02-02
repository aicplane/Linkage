# 模块五 --------------------------------------------------------------------

visNetworkTable <- reactiveVal(NULL)

observeEvent(input$submit6, {
  tryCatch({
    gene.list <- input$gene_list
    gene.list <- unlist(strsplit(gene.list, "\n"))
    
    if (input$genelist_idtype == "1") {
      index <- "ensembl_gene_id"
    }
    if (input$genelist_idtype == "2") {
      index <- "external_gene_name"
    }
    if (input$genelist_idtype == "3") {
      index <- "entrezgene_id"
    }
    
    if (length(intersect(gene.list, RNA_count()[, index])) != 0) {
      RNA.seq <- RNA_count()[RNA_count()[, index] %in% gene.list,]
      print(length(intersect(gene.list, RNA_count()[, index])))
    }
    
    else if (length(intersect(gene.list, RNA_count()[, index])) == 0) {
      print(length(intersect(gene.list, RNA_count()[, index])))
      sendSweetAlert(
        session = session,
        title = "ERROR",
        text = "Please enter the same value as your transformed gene ID and separate it with the Enter key!",
        type = "error"
      )
      RNA.seq <- NULL
    }
    
    progressSweetAlert(
      session = session,
      id = "simulationProgress",
      title = "Loading transcription factor database...",
      display_pct = TRUE,
      value = 0
    )
    
    motif <- list()
    result_peak <- list()
    result_p <- list()
    result_r <- list()
    result_FDR <- list()
    tf <- list()
    Gene.TF <- list()
    Gene.TF.frame <- data.frame()
    
    updateProgressBar(
      session = session,
      id = "simulationProgress",
      title = "Calculating highly correlated peaks...",
      value = 10
    )
    
    ATAC.seq <- ATAC_count()
    n <- 0
    for (i in 1:nrow(RNA.seq)) {
      RNA_row <- RNA.seq[i,]
      result_peak[[i]] <-
        ATAC.seq[RNA_row$chromosome_name == ATAC.seq$chrom &
                   ATAC.seq$chromStart > RNA_row$start_position - input$bins &
                   ATAC.seq$chromEnd < RNA_row$end_position + input$bins, ]
      p <- c()
      r <- c()
      for (j in 1:nrow(result_peak[[i]])) {
        df <- rbind(result_peak[[i]][j, c(-1:-3)],
                    RNA_row[, c(-1:-6)]) # 构造新的数据框
        tdf <- t(df)
        ftdf <- data.frame(tdf) # 转置数据框
        
        tryCatch({
          if (input$method == "1") {
            p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "pearson")$p.value
          }
          if (input$method == "2") {
            p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "spearman",exact=FALSE)$p.value
          }
          if (input$method == "3") {
            p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "kendall")$p.value
          }
        },
        error = function(e) {
          message("发生了错误: ", conditionMessage(e))
          p[j] <- NULL
        })
        tryCatch({
          if (input$method == "1") {
            r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "pearson")$estimate
          }
          if (input$method == "2") {
            r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "spearman",exact=FALSE)$estimate
          }
          if (input$method == "3") {
            r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "kendall")$estimate
          }
        },
        error = function(e) {
          message("发生了错误: ", conditionMessage(e))
          r[j] <- NULL
        })
      }
      result_peak[[i]]$p_value <- p
      result_peak[[i]]$FDR <- p.adjust(p, method = "BH")
      result_peak[[i]]$rho <- r
      
      n <- n + 20 / nrow(RNA.seq)
      
      updateProgressBar(
        session = session,
        id = "simulationProgress",
        title = "Calculating highly correlated peaks...",
        value = 10 + n
      )
    }
    updateProgressBar(
      session = session,
      id = "simulationProgress",
      title = "Performing motif enrichment analysis. It will take some time, please be patient and wait...",
      value = 30
    )
    
    n <- 0
    if (input$Species == "1") {
      PFMatrixList <- readRDS("../extdata/PFMatrixList.rds")
      pwm_library_dt <- readRDS("../extdata/pwm_library_dt.rds")
      genome <- "hg38"
    }
    if (input$Species == "2") {
      PFMatrixList <- readRDS("../extdata/Mus.PFMatrixList.rds")
      pwm_library_dt <- readRDS("../extdata/Mus.pwm_library_dt.rds")
      genome <- "mm10"
    }
    for (i in 1:length(result_peak)) {
      if (input$filter_method == "FDR") {
        peaks <-
          result_peak[[i]][result_peak[[i]]$"FDR" <= input$value, ]
      }
      if (input$filter_method == "rho") {
        peaks <- result_peak[[i]][result_peak[[i]]$rho >= input$value,]
      }
      if (input$filter_method == "p_value") {
        peaks <- result_peak[[i]][result_peak[[i]]$p_value <= input$value,]
      }
      peaks <- GRanges(
        seqnames = c(peaks$chrom),
        ranges = IRanges(
          start = peaks$chromStart,
          end = peaks$chromEnd
        )
      )
      
      # Get motif positions within peaks for example motifs in peaks
      motif_ix <- matchMotifs(PFMatrixList,
                              peaks,
                              genome = genome,
                              out = "positions") %>% data.frame()
      
      motif[[i]] <- pwm_library_dt[motif_ix$group, ]
      motif[[i]] <- cbind(motif[[i]], motif_ix)
      motif[[i]] <- unique(motif[[i]]$name)
      print(motif[[i]])
      
      tf[[i]] <-
        RNA_count()[RNA_count()$external_gene_name %in% motif[[i]],]
      RNA_row <- RNA.seq[i,]
      
      p <- c()
      r <- c()
      
      for (j in 1:nrow(tf[[i]])) {
        df <- rbind(tf[[i]][j, c(-1:-6)],
                    RNA_row[, c(-1:-6)]) # 构造新的数据框
        tdf <- t(df)
        ftdf <- data.frame(tdf) # 转置数据框
        
        tryCatch({
          if (input$TF_cor_method == "1") {
            p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "pearson")$p.value
          }
          if (input$TF_cor_method == "2") {
            p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "spearman",exact=FALSE)$p.value
          }
          if (input$TF_cor_method == "3") {
            p[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "kendall")$p.value
          }
        },
        error = function(e) {
          message("发生了错误: ", conditionMessage(e))
          p[j] <- NULL
        })
        tryCatch({
          if (input$TF_cor_method == "1") {
            r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "pearson")$estimate
          }
          if (input$TF_cor_method == "2") {
            r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "spearman",exact=FALSE)$estimate
          }
          if (input$TF_cor_method == "3") {
            r[j] <- cor.test(ftdf[, 1], ftdf[, 2], method = "kendall")$estimate
          }
        },
        error = function(e) {
          message("发生了错误: ", conditionMessage(e))
          r[j] <- NULL
        })
      }
      
      tf[[i]]$p_value <- p
      tf[[i]]$FDR <- p.adjust(p, method = "BH")
      tf[[i]]$rho <- r
      
      n <- n + 50 / length(result_peak)
      updateProgressBar(
        session = session,
        id = "simulationProgress",
        title = "Performing motif enrichment analysis. It will take some time, please be patient and wait...",
        value = 30 + n
      )
    }
    
    
    for (i in 1:nrow(RNA.seq)) {
      if (nrow(tf[[i]]) == 0) {
        Gene.TF[[i]] <- data.frame(
          gene = RNA.seq[i, index],
          TF = NA,
          p_value = NA,
          FDR = NA,
          rho = NA
        )
      }
      else{
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
    write.table(Gene.TF.frame, "Gene.TF.frame.txt")
    visNetworkTable(Gene.TF.frame)
    print("1.................................")
    print(head(Gene.TF.frame))
    
    updateProgressBar(
      session = session,
      id = "simulationProgress",
      title = "Building network diagram...",
      value = 90
    )
    if (input$TF_filter_method == "p_value") {
      Gene.TF.frame.filter <-
        Gene.TF.frame[(
          rowSums(is.na(Gene.TF.frame)) == 0 &
            Gene.TF.frame$p_value <= input$TF_cor_value &
            Gene.TF.frame$rho != 1
        ) |
          (rowSums(is.na(Gene.TF.frame)) == 4),]
    }
    if (input$TF_filter_method == "FDR") {
      Gene.TF.frame.filter <-
        Gene.TF.frame[(
          rowSums(is.na(Gene.TF.frame)) == 0 &
            Gene.TF.frame$FDR <= input$TF_cor_value &
            Gene.TF.frame$rho != 1
        ) |
          (rowSums(is.na(Gene.TF.frame)) == 4),]
    }
    if (input$TF_filter_method == "rho") {
      Gene.TF.frame.filter <-
        Gene.TF.frame[(
          rowSums(is.na(Gene.TF.frame)) == 0 &
            Gene.TF.frame$rho >= input$TF_cor_value &
            Gene.TF.frame$rho != 1
        ) |
          (rowSums(is.na(Gene.TF.frame)) == 4),]
    }
    output$Gene.TF.Table <- renderUI({
      shinydashboard::box(
        title = tagList(icon("gears"), "Gene TF Table"),
        width = 9,
        solidHeader = F,
        status = "primary",
        collapsible = TRUE,
        fluidRow(column(12,
                        dataTableOutput("visTable"))),
        fluidRow(
          column(
            4,
            radioButtons(
              inputId = "extTable4",
              label = helpText("Table output format"),
              choices = c("CSV" = "csv", "TXT" = "txt"),
              inline = T
            )
          ),
          column(4,
                 div(
                   downloadButton("Download_vis", "Download"),
                   shiny::tags$style(
                     "#Download_vis {background-color: white; color: black; margin-left: 30%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                   ),
                 )),
          column(
            4,
            do.call(actionBttn, c(
              list(
                inputId = "submit8",
                label = "Go Next",
                icon = icon("play")
              ),
              actionBttnParams
            )),
            shiny::tags$style(
              "#submit8 {margin-top: 10%;box-shadow: 0px 2px 5px #888888;}"
            ),
          )
        )
      )
    })
    
    updateProgressBar(
      session = session,
      id = "simulationProgress",
      title = "Building network diagram...",
      value = 100
    )
    
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = "DONE",
      text = "Network construction completed!",
      type = "success"
    )
  }, error = function(e) {
    print(e)
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "No highly correlated transcription factors were found for all genes!",
      type = "error"
    )
    visNetworkTable(NULL)
  })
})


BuildNetwork <- reactive({
  tryCatch({
    if (is.null(visNetworkTable())) {
      return()
    }
    Gene.TF.frame <- visNetworkTable()
    if (input$TF_filter_method == "p_value") {
      Gene.TF.frame.filter <-
        Gene.TF.frame[(
          rowSums(is.na(Gene.TF.frame)) == 0 &
            Gene.TF.frame$p_value <= input$TF_cor_value &
            Gene.TF.frame$rho != 1
        ) |
          (rowSums(is.na(Gene.TF.frame)) == 4),]
    }
    if (input$TF_filter_method == "FDR") {
      Gene.TF.frame.filter <-
        Gene.TF.frame[(
          rowSums(is.na(Gene.TF.frame)) == 0 &
            Gene.TF.frame$FDR <= input$TF_cor_value &
            Gene.TF.frame$rho != 1
        ) |
          (rowSums(is.na(Gene.TF.frame)) == 4),]
    }
    if (input$TF_filter_method == "rho") {
      Gene.TF.frame.filter <-
        Gene.TF.frame[(
          rowSums(is.na(Gene.TF.frame)) == 0 &
            Gene.TF.frame$rho >= input$TF_cor_value &
            Gene.TF.frame$rho != 1
        ) |
          (rowSums(is.na(Gene.TF.frame)) == 4),]
    }
    print("2.................................")
    print(head(Gene.TF.frame.filter))
    
    return(Gene.TF.frame.filter)
    
    if (nrow(Gene.TF.frame.filter) == 0) {
      sendSweetAlert(
        session = session,
        title = "ERROR",
        text = "No highly correlated transcription factors were found for all genes!",
        type = "error"
      )
      return()
    }
  }, error = function(e) {
    print(e)
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "No highly correlated transcription factors were found for all genes!",
      type = "error"
    )
    return()
  })
})

observe({
  if (is.null(BuildNetwork())) {
    output$visTable <- DT::renderDataTable(NULL)
  } else{
    output$visTable <-
      DT::renderDataTable({
        df <- BuildNetwork()
        # Create 19 breaks and 20 rgb color values ranging from white to blue
        brks1 <-
          quantile(df$rho,
                   probs = seq(.05, .95, .05),
                   na.rm = TRUE)
        brks2 <-
          quantile(df$FDR,
                   probs = seq(.05, .95, .05),
                   na.rm = TRUE)
        brks3 <-
          quantile(df$p_value,
                   probs = seq(.05, .95, .05),
                   na.rm = TRUE)
        # clrs <- round(seq(0, 100, length.out = length(brks) + 1), 0) %>%
        #   {paste0("rgb(", .,",", .,",220)")}
        DT::datatable(
          df,
          selection = "single",
          # extensions = c("Scroller", "RowReorder"),
          option = list(
            pageLength = 10,
            autoWidth = F,
            searchHighlight = TRUE,
            # columnDefs = list(list(
            #   targets = 2, width = "210px"
            # )),
            scrollX = TRUE
          )
        ) %>%
          formatStyle(names(df)[5], backgroundColor = styleInterval(brks1, head(Blues, n = length(brks1) + 1))) %>%
          formatStyle(names(df)[4], backgroundColor = styleInterval(brks2, Blues[(length(brks3) + 1):1])) %>%
          formatStyle(names(df)[3], backgroundColor = styleInterval(brks3, Blues[(length(brks3) + 1):1]))
      })
  }
})

# output$visTable <-
#   DT::renderDataTable(
#     BuildNetwork(),
#     selection = "none",
#     extensions = c("Scroller", "RowReorder"),
#     option = list(
#       rowReorder = TRUE,
#       deferRender = TRUE,
#       scrollY = 350,
#       scroller = TRUE,
#       scrollX = F,
#       searchHighlight = TRUE,
#       orderClasses = TRUE,
#       autoWidth = F
#     )
#   )

visNetWork <- reactive({
  tryCatch({
    if (is.null(BuildNetwork())) {
      return()
    }
    if (input$genelist_idtype == "1") {
      index <- "ensembl_gene_id"
    }
    if (input$genelist_idtype == "2") {
      index <- "external_gene_name"
    }
    if (input$genelist_idtype == "3") {
      index <- "entrezgene_id"
    }
    
    Gene.TF.frame <- visNetworkTable()
    Gene.TF.frame.filter <- BuildNetwork()
    Gene.TF.frame.filter <-
      dplyr::select(Gene.TF.frame.filter,
                    c("gene", "TF", "p_value", "FDR", "rho"))
    print("6.................................")
    print(head(Gene.TF.frame.filter))
    
    #if(nrow(Gene.TF.frame[rowSums(is.na(Gene.TF.frame)) == 4,]) > 0){
    Gene.TF.frame.filter <-
      rbind(Gene.TF.frame.filter, Gene.TF.frame[rowSums(is.na(Gene.TF.frame)) == 6, c("gene", "TF", "p_value", "FDR", "rho")], use.names =
              FALSE)
    #}
    Gene.TF.frame.filter <-
      Gene.TF.frame.filter[-which(Gene.TF.frame.filter$gene == FALSE),]
    print("7.................................")
    print(head(Gene.TF.frame.filter))
    
    # Gene.TF.frame.filter <- Gene.TF.frame.filter[,c("gene","TF",input$TF_filter_method)]
    negative.TF <-
      Gene.TF.frame.filter[Gene.TF.frame.filter$rho < 0,]$TF
    positive.TF <-
      Gene.TF.frame.filter[Gene.TF.frame.filter$rho > 0,]$TF
    
    Gene <- Gene.TF.frame.filter %>%
      distinct(gene) %>%
      dplyr::rename(label = gene)
    print("8.................................")
    print(head(Gene))
    
    #目的地去重
    TFs <- Gene.TF.frame.filter %>%
      distinct(TF) %>%
      dplyr::rename(label = TF)
    print("9.................................")
    print(head(TFs))
    
    ## 合并数据并添加一列索引
    nodes <- rbind(Gene, TFs)
    nodes <- nodes %>%
      mutate(id = 1:nrow(nodes)) %>%
      dplyr::select(id, everything())
    nodes <- nodes[!duplicated(nodes$label),]
    #id has to be the same like from and to columns in edges
    nodes$id <- nodes$label
    print(".....0")
    print(nodes)
    edges <- Gene.TF.frame.filter
    colnames(edges)[c(1:2)] <- c("from", "to")
    
    TF_filter_method <- input$TF_filter_method
    print(TF_filter_method)
    # print(TF_filter_method)
    # print(edges$`TF_filter_method`)
    # if (TF_filter_method == "p_value" | TF_filter_method == "FDR") {
    #   edges$value <- (1 / edges[[TF_filter_method]])
    # }
    # if (TF_filter_method == "rho") {
    edges$value <- abs(edges[["rho"]])
    # }
    print(head(edges))
    #Create graph for Louvain
    graph <- graph_from_data_frame(edges, directed = FALSE)
    #Louvain Comunity Detection
    cluster <- cluster_louvain(graph)
    cluster_df <- data.frame(as.list(membership(cluster)))
    cluster_df <- as.data.frame(t(cluster_df))
    if (input$genelist_idtype == "3") {
      rownames(cluster_df) <- str_sub(rownames(cluster_df), 2)
    }
    cluster_df$label <- rownames(cluster_df)
    #Create group column
    print("......c")
    print(cluster_df)
    print(nodes)
    
    nodes <- left_join(nodes, cluster_df, by = "label")
    print(".....1")
    print(nodes)
    print(".....2")
    print(nodes)
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
             "lightblue")
    tryCatch({
      if (length(which(rowSums(is.na(nodes)) > 0)) > 0) {
        nodes <- nodes[-which(rowSums(is.na(nodes)) > 0),]
      }
      RNA_count <- RNA_count()
      # print(RNA_count[-which(duplicated(RNA_count[[index]])),])
      RNA_count <-
        RNA_count[-which(duplicated(RNA_count[[index]])),]
      nodes$value <-
        apply(RNA_count[RNA_count[[index]] %in% nodes$label, c(-1:-6)], 1, mean)
    }, error = function(e) {
      print(e)
      nodes$value <- 1
    })
    
    print(nodes)
    if (length(which(rowSums(is.na(edges)) > 0))) {
      edges <- edges[-which(rowSums(is.na(edges)) > 0),]
    }
    print(edges)
    edges$color <-
      ifelse(edges$to %in% positive.TF, "#FF8C00", "lightgreen")
    
    network <- visNetwork(nodes, edges) %>%
      visGroups(groupname = "Gene", color = "red") %>%
      visGroups(groupname = "Negative TF", color = "lightblue") %>%
      visGroups(groupname = "Positive TF", color = "lightblue") %>%
      visIgraphLayout() %>%
      visEdges(arrows = 'from') %>%
      visInteraction(
        navigationButtons = TRUE,
        dragNodes = T,
        dragView = T,
        zoomView = F
      ) %>%
      visOptions(
        highlightNearest = T,
        nodesIdSelection = TRUE,
        selectedBy = "group"
      )
    return(network)
  }, error = function(e) {
    print(e)
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "No highly correlated transcription factors were found for all genes!",
      type = "error"
    )
    return()
  })
})

observe({
  if (is.null(visNetWork()) == F) {
    output$visNetwork <- renderUI({
      shinydashboard::box(
        title = tagList(icon("chart-column"), "Network"),
        width = 12,
        solidHeader = F,
        status = "primary",
        collapsible = TRUE,
        visNetworkEditorUI(
          id = paste0(
            "id.build",
            input$submit,
            input$submit6,
            input$TF_filter_method,
            input$TF_cor_value,
            input$TF_cor_method,
            input$genelist_idtype
          ),
          quitButton = FALSE
        ) %>% withSpinner(color = "#3c8cbc")
      )
    })
    shiny::callModule(
      visNetworkEditorServer,
      paste0(
        "id.build",
        input$submit,
        input$submit6,
        input$TF_filter_method,
        input$TF_cor_value,
        input$TF_cor_method,
        input$genelist_idtype
      ),
      object = shiny::reactive(visNetWork())
    )
  }
})

input_networkdata <- reactiveVal(NULL)
observeEvent(input$Build, {
  tryCatch({
    input_networkdata(fread(input$Network_data$datapath))
    
    output$Gene.TF.Table <- renderUI({
      shinydashboard::box(
        title = tagList(icon("gears"), "Gene TF Table"),
        width = 9,
        solidHeader = F,
        status = "primary",
        collapsible = TRUE,
        fluidRow(column(12,
                        dataTableOutput("visTable"))),
        fluidRow(
          column(
            4,
            radioButtons(
              inputId = "extTable4",
              label = helpText("Table output format"),
              choices = c("CSV" = "csv", "TXT" = "txt"),
              inline = T
            )
          ),
          column(4,
                 div(
                   downloadButton("Download_vis", "Download"),
                   shiny::tags$style(
                     "#Download_vis {background-color: white; color: black; margin-left: 30%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                   ),
                 )),
          column(
            4,
            do.call(actionBttn, c(
              list(
                inputId = "submit8",
                label = "Go Next",
                icon = icon("play")
              ),
              actionBttnParams
            )),
            shiny::tags$style(
              "#submit8 {margin-top: 10%;box-shadow: 0px 2px 5px #888888;}"
            ),
          )
        )
      )
    })
    
  }, error = function(e) {
    print(e)
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "Please select the correct file to upload!",
      type = "error"
    )
    input_networkdata(NULL)
  })
})

upload_filter <- reactive({
  tryCatch({
    if (is.null(input_networkdata())) {
      return()
    }
    Gene.TF.frame <- input_networkdata()
    
    if (input$TF_filter_method2 == "p_value") {
      Gene.TF.frame.filter <-
        Gene.TF.frame[(
          rowSums(is.na(Gene.TF.frame)) == 0 &
            Gene.TF.frame$p_value <= input$TF_cor_value2 &
            Gene.TF.frame$rho != 1
        ) |
          (rowSums(is.na(Gene.TF.frame)) == 4),]
    }
    if (input$TF_filter_method2 == "FDR") {
      Gene.TF.frame.filter <-
        Gene.TF.frame[(
          rowSums(is.na(Gene.TF.frame)) == 0 &
            Gene.TF.frame$FDR <= input$TF_cor_value2 &
            Gene.TF.frame$rho != 1
        ) |
          (rowSums(is.na(Gene.TF.frame)) == 4),]
    }
    if (input$TF_filter_method2 == "rho") {
      Gene.TF.frame.filter <-
        Gene.TF.frame[(
          rowSums(is.na(Gene.TF.frame)) == 0 &
            Gene.TF.frame$rho >= input$TF_cor_value2 &
            Gene.TF.frame$rho != 1
        ) |
          (rowSums(is.na(Gene.TF.frame)) == 4),]
    }
    
    Gene.TF.frame.filter <-
      dplyr::select(Gene.TF.frame.filter,
                    c("gene", "TF", "p_value", "FDR", "rho"))
    return(Gene.TF.frame.filter)
  }, error = function(e) {
    print(e)
  })
})

upload_network <- reactive({
  if (is.null(upload_filter())) {
    return()
  }
  tryCatch({
    if (input$genelist_idtype2 == "1") {
      index <- "ensembl_gene_id"
    }
    if (input$genelist_idtype2 == "2") {
      index <- "external_gene_name"
    }
    if (input$genelist_idtype2 == "3") {
      index <- "entrezgene_id"
    }
    
    Gene.TF.frame <- input_networkdata()
    Gene.TF.frame.filter <- upload_filter()
    Gene.TF.frame.filter <-
      rbind(Gene.TF.frame.filter, Gene.TF.frame[rowSums(is.na(Gene.TF.frame)) == 6, c("gene", "TF", "p_value", "FDR", "rho")], use.names = FALSE)
    
    if (length(which(Gene.TF.frame.filter$gene == FALSE)) > 0) {
      Gene.TF.frame.filter <-
        Gene.TF.frame.filter[-which(Gene.TF.frame.filter$gene == FALSE),]
    }
    
    print("7.................................")
    print(head(Gene.TF.frame.filter))
    
    # Gene.TF.frame.filter <- Gene.TF.frame.filter[,c("gene","TF",input$TF_filter_method)]
    negative.TF <-
      Gene.TF.frame.filter[Gene.TF.frame.filter$rho < 0,]$TF
    positive.TF <-
      Gene.TF.frame.filter[Gene.TF.frame.filter$rho > 0,]$TF
    
    Gene <- Gene.TF.frame.filter %>%
      distinct(gene) %>%
      dplyr::rename(label = gene)
    print("8.................................")
    print(head(Gene))
    
    #目的地去重
    TFs <- Gene.TF.frame.filter %>%
      distinct(TF) %>%
      dplyr::rename(label = TF)
    print("9.................................")
    print(head(TFs))
    
    ## 合并数据并添加一列索引
    nodes <- rbind(Gene, TFs)
    # nodes <- nodes[!duplicated(nodes),]
    nodes <- nodes %>%
      mutate(id = 1:nrow(nodes)) %>%
      dplyr::select(id, everything())
    # head(nodes, 2)
    nodes <- nodes[!duplicated(nodes$label),]
    #id has to be the same like from and to columns in edges
    nodes$id <- nodes$label
    # head(nodes)
    print(".....0")
    print(nodes)
    edges <- Gene.TF.frame.filter
    colnames(edges)[c(1:2)] <- c("from", "to")
    
    TF_filter_method <- input$TF_filter_method2
    # print(TF_filter_method)
    # print(edges$`TF_filter_method`)
    # if (TF_filter_method == "p_value" | TF_filter_method == "FDR") {
    #   edges$value <- (1 / edges[[TF_filter_method]])
    # }
    # if (TF_filter_method == "rho") {
    edges$value <- abs(edges[["rho"]])
    # }
    # edges$value <- runif(nrow(edges), min = 1, max = 10)
    print(head(edges))
    #Create graph for Louvain
    graph <- graph_from_data_frame(edges, directed = FALSE)
    #Louvain Comunity Detection
    cluster <- cluster_louvain(graph)
    cluster_df <- data.frame(as.list(membership(cluster)))
    cluster_df <- as.data.frame(t(cluster_df))
    if (input$genelist_idtype2 == "3") {
      rownames(cluster_df) <- str_sub(rownames(cluster_df), 2)
    }
    cluster_df$label <- rownames(cluster_df)
    #Create group column
    print("......c")
    print(cluster_df)
    print(nodes)
    
    nodes <- left_join(nodes, cluster_df, by = "label")
    print(".....1")
    print(nodes)
    print(".....2")
    print(nodes)
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
             "lightblue")
    print(nodes)
    tryCatch({
      if (length(which(rowSums(is.na(nodes)) > 0)) > 0) {
        nodes <- nodes[-which(rowSums(is.na(nodes)) > 0),]
      }
      RNA_count <- RNA_count()
      # print(RNA_count[-which(duplicated(RNA_count[[index]])),])
      RNA_count <-
        RNA_count[-which(duplicated(RNA_count[[index]])),]
      nodes$value <-
        apply(RNA_count[RNA_count[[index]] %in% nodes$label, c(-1:-6)], 1, mean)
    }, error = function(e) {
      print(e)
      nodes$value <- 1
    })
    
    # nodes$color.border <- ifelse(nodes$label %in% positive.TF,"#FF8C00","lightgreen")
    print("edgrs......")
    print(nodes)
    
    # if(length(-which(rowSums(is.na(edges)) > 0))){
    #   edges <- edges[-which(rowSums(is.na(edges)) > 0),]
    # }
    print(edges)
    edges$color <-
      ifelse(edges$to %in% positive.TF, "#FF8C00", "lightgreen")
    
    network <- visNetwork(nodes, edges) %>%
      visGroups(groupname = "Gene", color = "red") %>%
      visGroups(groupname = "Negative TF", color = "lightblue") %>%
      visGroups(groupname = "Positive TF", color = "lightblue") %>%
      visIgraphLayout() %>%
      visEdges(arrows = 'from') %>%
      visInteraction(
        navigationButtons = TRUE,
        dragNodes = T,
        dragView = T,
        zoomView = F
      ) %>%
      visOptions(
        highlightNearest = T,
        nodesIdSelection = TRUE,
        selectedBy = "group"
      )
    
    return(network)
  }, error = function(e) {
    print(e)
    
    tryCatch({
      Gene.TF.frame.filter <- input_networkdata()
      print(head(colnames(Gene.TF.frame.filter)[1:2]))
      colnames(Gene.TF.frame.filter)[1:2] <- c("Target", "Source")
      # Gene.TF.frame.filter <-
      # rbind(Gene.TF.frame.filter, Gene.TF.frame[rowSums(is.na(Gene.TF.frame)) == 6, c("gene", "TF", "p_value", "FDR", "rho")], use.names = FALSE)
      
      if (length(which(Gene.TF.frame.filter$Source == FALSE)) > 0) {
        Gene.TF.frame.filter <-
          Gene.TF.frame.filter[-which(Gene.TF.frame.filter$Source == FALSE),]
      }
      
      print("7.................................")
      print(head(Gene.TF.frame.filter))
      
      # # Gene.TF.frame.filter <- Gene.TF.frame.filter[,c("gene","TF",input$TF_filter_method)]
      # negative.TF <-
      #   Gene.TF.frame.filter[Gene.TF.frame.filter$rho < 0, ]$TF
      # positive.TF <-
      #   Gene.TF.frame.filter[Gene.TF.frame.filter$rho > 0, ]$TF
      
      Gene <- Gene.TF.frame.filter %>%
        distinct(Target) %>%
        dplyr::rename(label = Target)
      print("8.................................")
      print(head(Gene))
      
      #目的地去重
      TFs <- Gene.TF.frame.filter %>%
        distinct(Source) %>%
        dplyr::rename(label = Source)
      print("9.................................")
      print(head(TFs))
      
      ## 合并数据并添加一列索引
      nodes <- rbind(Gene, TFs)
      # nodes <- nodes[!duplicated(nodes),]
      nodes <- nodes %>%
        mutate(id = 1:nrow(nodes)) %>%
        dplyr::select(id, everything())
      # head(nodes, 2)
      nodes <- nodes[!duplicated(nodes$label),]
      #id has to be the same like from and to columns in edges
      nodes$id <- nodes$label
      # head(nodes)
      print(".....0")
      print(nodes)
      edges <- Gene.TF.frame.filter
      colnames(edges)[c(1:2)] <- c("from", "to")
      
      print(head(edges))
      #Create graph for Louvain
      graph <- graph_from_data_frame(edges, directed = FALSE)
      #Louvain Comunity Detection
      cluster <- cluster_louvain(graph)
      cluster_df <- data.frame(as.list(membership(cluster)))
      cluster_df <- as.data.frame(t(cluster_df))
      cluster_df$label <- rownames(cluster_df)
      #Create group column
      print("......c")
      print(cluster_df)
      print(nodes)
      
      nodes <- left_join(nodes, cluster_df, by = "label")
      print(".....1")
      print(nodes)
      print(".....2")
      print(nodes)
      colnames(nodes)[3] <- "group"
      
      nodes$color <-
        ifelse(nodes$id %in% Gene.TF.frame.filter$Source,
               "red",
               "lightblue")
      print(nodes)
      
      # nodes$color.border <- ifelse(nodes$label %in% positive.TF,"#FF8C00","lightgreen")
      print("edgrs......")
      print(nodes)
      
      network <- visNetwork(nodes, edges) %>%
        visIgraphLayout() %>%
        visInteraction(
          navigationButtons = TRUE,
          dragNodes = T,
          dragView = T,
          zoomView = F
        ) %>%
        visOptions(
          highlightNearest = T,
          nodesIdSelection = TRUE,
          selectedBy = "group"
        )
      
      return(network)
    }, error = function(e) {
      print(e)
    })
    
  })
})

observe({
  if (is.null(upload_network()) == F) {
    output$visTable <-
      DT::renderDataTable(
        input_networkdata(),
        selection = "none",
        extensions = c("Scroller", "RowReorder"),
        option = list(
          rowReorder = TRUE,
          deferRender = TRUE,
          scrollY = 350,
          scroller = TRUE,
          scrollX = F,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          autoWidth = F
        )
      )
    output$visNetwork <- renderUI({
      shinydashboard::box(
        title = tagList(icon("chart-column"), "Network"),
        width = 12,
        solidHeader = F,
        status = "primary",
        collapsible = TRUE,
        visNetworkEditorUI(
          id = paste0(
            "id.build",
            input$submit,
            input$submit6,
            input$Build,
            input$TF_filter_method2,
            input$TF_cor_value2,
            input$TF_cor_method,
            input$genelist_idtype2
          ),
          quitButton = FALSE
        ) %>% withSpinner(color = "#3c8cbc")
      )
    })
    shiny::callModule(
      visNetworkEditorServer,
      paste0(
        "id.build",
        input$submit,
        input$submit6,
        input$Build,
        input$TF_filter_method2,
        input$TF_cor_value2,
        input$TF_cor_method,
        input$genelist_idtype2
      ),
      object = shiny::reactive(upload_network())
    )
  }
})
