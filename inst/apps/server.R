server <- function(input, output, session) {
  # 选择数据，动态输出文件上传控件、按钮控件 -----------------------------------
  shinyjs::disable(selector = ".sidebar li a[data-value='one']")
  shinyjs::disable(selector = ".sidebar li a[data-value='tow']")
  shinyjs::disable(selector = ".sidebar li a[data-value='three']")
  shinyjs::disable(selector = ".sidebar li a[data-value='four']")
  shinyjs::disable(selector = ".sidebar li a[data-value='five']")
  shinyjs::disable(selector = ".sidebar li a[data-value='six']")

  observe({
    if (is.null(input$data)) {
      return()
    }
    if (input$data == "1") {
      shinyWidgets::updateRadioGroupButtons(session, "Species", selected = "1")
      shinyjs::toggleState(id = "Species", condition = input$data == "3")
    }
    if (input$data == "2") {
      shinyWidgets::updateRadioGroupButtons(session, "Species", selected = "2")
      shinyjs::toggleState(id = "Species", condition = input$data == "3")
    }

  })

  observeEvent(input$submit2, {
    shinyjs::enable(selector = ".sidebar li a[data-value='tow']")
    js.2 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=tow]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.2)
  })

  observeEvent(input$submit3, {
    shinyjs::enable(selector = ".sidebar li a[data-value='three']")
    js.3 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=three]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.3)
  })

  observeEvent(input$submit4, {
    shinyjs::enable(selector = ".sidebar li a[data-value='four']")
    js.4 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.4)
  })

  observeEvent(input$submit5, {
    shinyjs::enable(selector = ".sidebar li a[data-value='five']")
    js.5 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.5)

    shinyjs::enable(selector = ".sidebar li a[data-value='six']")
    js.6 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.6)
  })

  observe({
    if (input$data %in% c("1","2")) {
      output$datahelptext <- renderUI({
        tagList(
          br(),
          br(),
          div(
            "The",strong("TCGA Breast Cancer cohort"),
            "dataset contains chromatin accessibility and gene expression data from ",
            code("72 patients with breast cancer"),".",
            "The chromatin accessibility data is a normalized bulk ATAC-seq count matrix, which a prior count of 5 is added to the raw counts, then put into a \"counts per million\", then log2 transformed, then quantile normalized. The gene expression data is a normalized bulk RNA-seq count matrix, which is log2(fpkm+1) transformed.",
            align = "justify"
          ),
          br(),
          br(),
          div(
            "The",strong("GSE121589"),
            "dataset contains chromatin accessibility and gene expression data from ",
            code("33 mice Muscle Stem Cells (MuSCs)"),".",
            "The chromatin accessibility data is a normalized bulk ATAC-seq count matrix, which a prior count of 5 is added to the raw counts, then put into a \"counts per million\", then log2 transformed, then quantile normalized. The gene expression data is a normalized bulk RNA-seq count matrix, which is log2(fpkm+1) transformed.",
            align = "justify"
          )
        )
      })
    }

    if (input$data == "3") {
      output$datahelptext <- renderUI({
        tagList(
          div(
            code("Two tab-delimited text/csv input files (chromatin accessibility matrix and gene expression matrix) are required before running Linkage."),
            "The gene expression matrix file is a tab-delimited multi-column data matrix, which the first column represents gene symbols and the following columns represent normalized or raw expression levels of genes for each sample. The chromatin accessibility matrix file is a tab-delimited multi-column data matrix as well, which the first three columns represent chromosome name, start coordinate on the chromosome and end coordinate on the chromosome of the peaks respectively; the remaining columns of the chromatin accessibility matrix file represent normalized or raw chromatin accessibility levels of peaks for each sample.",
            strong("Each element of ready-to-analysis files will appear in Chromatin Accessibility Matrix panel and Gene Expression Matrix panel."),
            align = "justify"
          )
        )
      })
    }
  })

  output$Fileinput <- renderUI({
    if (is.null(req(input$data))) {
      return()
    }
    switch(req(input$data),
           "3" = tagList(
             fluidRow(column(12, strong("Data input"))),

             fluidRow(column(6,
                             code("ATAC-seq:")),
                      column(6, code("RNA-seq:"))),
             fluidRow(column(
               6,
               fileInput("ATAC_data",
                         label = NULL)
             ),
             column(6,
                    fileInput(
                      "RNA_data",
                      label = NULL
                    )))
           ))
  })

  output$next_1 <- renderUI({
    if (is.null(select_ATAC()) == FALSE) {
      #actionButton("submit2", "Go Next")
      do.call(actionBttn, c(
        list(
          inputId = "submit2",
          label = "Continue",
          icon = icon("play")
        ),
        actionBttnParams
      ))
    }
  })

  output$next_2 <- renderUI({
    if (is.null(select_boxplot()) == FALSE) {
      #div(actionButton("submit3", "Go Next"), align = "center")
      column(3, align = "center",
             do.call(actionBttn, c(
               list(
                 inputId = "submit3",
                 label = "Continue",
                 icon = icon("play")
               ),
               actionBttnParams
             )))
    }
  })

  output$next_3 <- renderUI({
    if (input$Annote > 0) {
      #actionButton("submit4", "Go Next")
      do.call(actionBttn, c(
        list(
          inputId = "submit4",
          label = "Continue",
          icon = icon("play")
        ),
        actionBttnParams
      ))
    }
  })

  output$next_4 <- renderUI({
    if (is.null(select_motif()) == FALSE) {
      #actionButton("submit5", "Go Next")
      column(3, align = "center",
             do.call(actionBttn, c(
               list(
                 inputId = "submit5",
                 label = "Continue",
                 icon = icon("play")
               ),
               actionBttnParams
             )))
    }
  })


  # 动态修改默认值 -----------------------------------------------------------------
  observe({
    if (is.null(input$data)) {
      return()
    }
    switch(
      input$data,
      "1" = updateSelectInput(session, "From",
                              choices = list("ENSEMBL")),
      "2" = updateSelectInput(session, "From",
                              choices = list("ENSEMBL")),
      "3" = updateSelectInput(session, "From",
                              choices = list("ENSEMBL",
                                             "SYMBOL",
                                             "ENTREZ"))
    )
  })


  observe({
    if (input$Species == "1") {
      if (is.null(input$geneid_method)) {
        return()
      }
      switch(
        input$geneid_method,
        "SYMBOL" = updateTextInput(session, "geneid",
                                   value = "DPM1"),
        "ENSEMBL" = updateTextInput(session, "geneid",
                                    value = "ENSG00000000419"),
        "ENTREZ" = updateTextInput(session, "geneid",
                                   value = "8813")
      )
    }
    if (input$Species == "2") {
      if (is.null(input$geneid_method)) {
        return()
      }
      switch(
        input$geneid_method,
        "SYMBOL" = updateTextInput(session, "geneid",
                                   value = "Acat3"),
        "ENSEMBL" = updateTextInput(session, "geneid",
                                    value = "ENSMUSG00000062480"),
        "ENTREZ" = updateTextInput(session, "geneid",
                                   value = "224530")
      )
    }
  })

  observe({
    if (is.null(input$filter_method)) {
      return()
    }
    switch(
      input$filter_method,
      "p_value" = updateTextInput(session, "value",
                                  value = "0.01"),
      "FDR" = updateTextInput(session, "value",
                              value = "0.01"),
      "rho" = updateTextInput(session, "value",
                              value = "0.5")
    )
  })

  observe({
    if (is.null(input$fil_method)) {
      return()
    }
    switch(
      input$fil_method,
      "p_value" = updateTextInput(session, "network_peak_filter",
                                  value = "0.01"),
      "FDR" = updateTextInput(session, "network_peak_filter",
                              value = "0.01"),
      "rho" = updateTextInput(session, "network_peak_filter",
                              value = "0.5")
    )
  })


  # 跳转页面 --------------------------------------------------------------------
  # observeEvent(input$submit, {
  #   updateTabItems(session, "inTabset", selected = "one")
  # })

  observeEvent(input$submit2, {
    updateTabItems(session, "inTabset", selected = "tow")
  })

  observeEvent(input$submit3, {
    updateTabItems(session, "inTabset", selected = "three")
  })

  observeEvent(input$submit4, {
    updateTabItems(session, "inTabset", selected = "four")
  })

  observeEvent(input$submit5, {
    updateTabItems(session, "inTabset", selected = "five")
  })

  observeEvent(input$submit8, {
    updateTabItems(session, "inTabset", selected = "six")
  })

  # 上传数据 --------------------------------------------------------------------
  read.d1 <- reactiveVal(NULL)

  observeEvent(input$RNA_data, {
    if (is.null(input$RNA_data)) {
      # 如果用户没有上传文件，使用已有的数据集替换
      read.d1(existing_dataset)
      # return()
    } else {
      # 如果用户上传了文件，读取上传的文件
      read.d1(fread(input$RNA_data$datapath))
    }
  })

  read.d2 <- reactiveVal(NULL)

  observeEvent(input$ATAC_data, {
    if (is.null(input$ATAC_data)) {
      # 如果用户没有上传文件，使用已有的数据集替换
      read.d2(existing_dataset)
      # return()
    } else {
      # 如果用户上传了文件，读取上传的文件
      read.d2(fread(input$ATAC_data$datapath))
    }
  })

  RNA_count <- reactiveVal(NULL)
  ATAC_count <- reactiveVal(NULL)
  gene_select <- reactiveVal(NULL)
  # 数据适配 --------------------------------------------------------------------
  observeEvent(input$submit, {
    RNA_count(NULL)
    ATAC_count(NULL)
    tryCatch({
      if (req(input$data) == "1") {
        RNA <- RNA_matrix
      }
      if (req(input$data) == "2") {
        RNA <- mouse.RNA_matrix
      }
      if (req(input$data) == "3") {
        RNA <- read.d1()
      }

      if (input$Species == "1") {
        gene.positions <- homo.gene.positions
      } else{
        gene.positions <- mus.gene.positions
      }
      gene.positions <- as.data.frame(gene.positions)
      gene.positions$entrezgene_id <-
        as.character(gene.positions$entrezgene_id)
      gene.positions$entrezgene_id <-
        ifelse(is.na(gene.positions$entrezgene_id),
               "",
               gene.positions$entrezgene_id)

      for (col_name in colnames(gene.positions)[c(1:3)]) {
        tryCatch({
          merge_df <-
            merge(
              gene.positions,
              RNA,
              by.x = col_name,
              by.y = colnames(RNA)[1],
              all.x = F,
              all.y = F
            )
          if (nrow(merge_df) > 0) {
            break
          }
        }, error = function(e) {
          print(e)
        })
      }
      merge_df <- as.data.frame(merge_df)
      RNA_count(merge_df)
    }, error = function(e) {
      sendSweetAlert(
        session = session,
        title = "ERROR",
        text = "Please upload the correct file!",
        type = "error"
      )
      print(e)
      RNA_count(NULL)
    })
    tryCatch({
      if (req(input$data) == "1") {
        ATAC <- ATAC_matrix
      }
      if (req(input$data) == "2") {
        ATAC <- mouse.ATAC_matrix
      }
      if (req(input$data) == "3") {
        ATAC <- read.d2()
      }
      if (ATAC[1, 2] < ATAC[1, 3]) {
        names(ATAC)[1] <- "chrom"
        names(ATAC)[2] <- "chromStart"
        names(ATAC)[3] <- "chromEnd"
      }
      if (ATAC[1, 2] > ATAC[1, 3]) {
        names(ATAC)[1] <- "chrom"
        names(ATAC)[2] <- "chromEnd"
        names(ATAC)[3] <- "chromStart"
      }
      ATAC_count(dplyr::select(ATAC, "chrom", "chromStart", "chromEnd", everything()))
      # return(ATAC_count())
    }, error = function(e) {
      sendSweetAlert(
        session = session,
        title = "ERROR",
        text = "Please upload the correct file!",
        type = "error"
      )
      print(e)
      ATAC_count(NULL)
    })
    if (is.null(RNA_count()) == F & is.null(ATAC_count()) == F) {
      shinyjs::enable(selector = ".sidebar li a[data-value='one']")
      js.1 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=one]").css("color", "#E6E7E8");
        });
      '
      shinyjs::runjs(js.1)

      updateTabItems(session, "inTabset", selected = "one")
    }
  })


  # 主页 --------------------------------------------------------

  observeEvent(input$data, {
    RNA_count(NULL)
    ATAC_count(NULL)
    # gene_select(NULL)
  })
  observe({
    if (is.null(RNA_count()) & input$data == "1") {
      output$eg_RNA <-
        DT::renderDataTable(
          RNA_matrix,
          selection = 'none',
          extensions = 'FixedHeader',
          options = list(
            pageLength = 10,
            autoWidth = F,
            searchHighlight = TRUE,
            # columnDefs = list(list(targets = 10, width = "210px")),
            scrollX = TRUE,
            FixedHeader = T
          )
        )
    }
    if (is.null(RNA_count()) & input$data == "2") {
      output$eg_RNA <-
        DT::renderDataTable(
          mouse.RNA_matrix,
          selection = 'none',
          options = list(
            pageLength = 10,
            autoWidth = F,
            searchHighlight = TRUE,
            # columnDefs = list(list(targets = 10, width = "210px")),
            scrollX = TRUE
          )
        )
    }
    if (is.null(RNA_count()) & input$data == "3") {
      output$eg_RNA <-
        DT::renderDataTable(
          NULL,
          selection = 'none',
          options = list(
            pageLength = 10,
            autoWidth = F,
            searchHighlight = TRUE,
            # columnDefs = list(list(targets = 10, width = "210px")),
            scrollX = TRUE
          )
        )
    }
    if (is.null(RNA_count()) == F) {
      output$eg_RNA <-
        DT::renderDataTable(
          RNA_count(),
          selection = 'none',
          options = list(
            pageLength = 10,
            autoWidth = F,
            searchHighlight = TRUE,
            # columnDefs = list(list(targets = 10, width = "210px")),
            scrollX = TRUE
          )
        )
    }
    if (is.null(ATAC_count()) & input$data == "1") {
      output$eg_ATAC <- DT::renderDataTable(
        ATAC_matrix,
        selection = 'none',
        options = list(
          pageLength = 10,
          autoWidth = F,
          searchHighlight = TRUE,
          #columnDefs = list(list(targets = 10, width = "210px")),
          scrollX = TRUE
        )
      )
    }
    if (is.null(ATAC_count()) & input$data == "2") {
      output$eg_ATAC <- DT::renderDataTable(
        mouse.ATAC_matrix,
        selection = 'none',
        options = list(
          pageLength = 10,
          autoWidth = F,
          searchHighlight = TRUE,
          #columnDefs = list(list(targets = 10, width = "210px")),
          scrollX = TRUE
        )
      )
    }
    if (is.null(ATAC_count()) & input$data == "3") {
      output$eg_ATAC <- DT::renderDataTable(
        NULL,
        selection = 'none',
        options = list(
          pageLength = 10,
          autoWidth = F,
          searchHighlight = TRUE,
          #columnDefs = list(list(targets = 10, width = "210px")),
          scrollX = TRUE
        )
      )
    }
    if (is.null(ATAC_count()) == F) {
      output$eg_ATAC <- DT::renderDataTable(
        RNA <- ATAC_count(),
        selection = 'none',
        options = list(
          pageLength = 10,
          autoWidth = F,
          searchHighlight = TRUE,
          #columnDefs = list(list(targets = 10, width = "210px")),
          scrollX = TRUE
        )
      )
    }
    if (is.null(RNA_count()) == T & is.null(read.d1()) == F) {
      output$eg_RNA <- DT::renderDataTable(
        RNA <- read.d1(),
        selection = 'none',
        options = list(
          pageLength = 10,
          autoWidth = F,
          searchHighlight = TRUE,
          #columnDefs = list(list(targets = 10, width = "210px")),
          scrollX = TRUE
        )
      )
    }
    if (is.null(ATAC_count()) == T & is.null(read.d2()) == F) {
      output$eg_ATAC <- DT::renderDataTable(
        RNA <- read.d2(),
        selection = 'none',
        options = list(
          pageLength = 10,
          autoWidth = F,
          searchHighlight = TRUE,
          #columnDefs = list(list(targets = 10, width = "210px")),
          scrollX = TRUE
        )
      )
    }
  })

  observe({
    if (is.null(RNA_count()) | is.null(ATAC_count())) {
      shinyjs::disable(selector = ".sidebar li a[data-value='one']")
      shinyjs::disable(selector = ".sidebar li a[data-value='tow']")
      shinyjs::disable(selector = ".sidebar li a[data-value='three']")
      shinyjs::disable(selector = ".sidebar li a[data-value='four']")
      shinyjs::disable(selector = ".sidebar li a[data-value='five']")
      shinyjs::disable(selector = ".sidebar li a[data-value='six']")

      js.1 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=one]").css("color", "#808080");
        });
      '
      shinyjs::runjs(js.1)
      js.2 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=tow]").css("color", "#808080");
        });
      '
      shinyjs::runjs(js.2)
      js.3 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=three]").css("color", "#808080");
        });
      '
      shinyjs::runjs(js.3)
      js.4 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#808080");
        });
      '
      shinyjs::runjs(js.4)
      js.5 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#808080");
        });
      '
      shinyjs::runjs(js.5)
      js.6 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#808080");
        });
      '
      shinyjs::runjs(js.6)
    }
  })


  # 模块一 --------------------------------------------------------

  output_gene <- reactive({
    if (is.null(RNA_count()))
      return()
    RNA <- RNA_count()

    if (is.null(RNA) == F) {
      if (input$geneid_method == "ENSEMBL") {
        gene <- RNA[RNA$ensembl_gene_id == req(input$geneid),]
      }
      if (input$geneid_method == "SYMBOL") {
        gene <- RNA[RNA$external_gene_name == req(input$geneid),]
      }
      if (input$geneid_method == "ENTREZ") {
        gene <- RNA[RNA$entrezgene_id == req(input$geneid),]
      }

      output_gene <- gene
      return(output_gene)
    }
  })

  # peak筛选
  select_ATAC <- reactive({
    if (is.null(ATAC_count()))
      return()
    tryCatch({
      if (input$bins > 1000000) {
        sendSweetAlert(
          session = session,
          title = "ERROR",
          text = "Search scope(±bp) must less 1,000,000!",
          type = "error"
        )
        return(NULL)
      }
      #scope <- as.numeric(req(input$bins))
      scope <- req(input$bins)
      if (is.null(ATAC_count())) {
        return(NULL)
      }
      else {
        gene <- output_gene()
        if (nrow(gene) == 0) {
          return()
        }
        df <- data.frame()
        Filter_value <- req(input$value)
        Filter_Method <- req(input$filter_method)

        ATAC <- ATAC_count()
        transform_gene <-
          transform(gene,
                    start_position = start_position - scope,
                    end_position = end_position + scope)
        ATAC1 <-
          ATAC[ATAC$chrom == transform_gene$chromosome_name,]

        ATAC2 <-
          ATAC1[ATAC1$chromStart > transform_gene$start_position &
                  ATAC1$chromEnd < transform_gene$end_position,]

        if (req(input$method) == "1") {
          return(cor_test(ATAC2,
                          gene,
                          "pearson",
                          Filter_Method,
                          Filter_value))
        }
        if (req(input$method) == "2") {
          return(cor_test(ATAC2,
                          gene,
                          "spearman",
                          Filter_Method,
                          Filter_value))
        }
        if (req(input$method) == "3") {
          return(cor_test(ATAC2,
                          gene,
                          "kendall",
                          Filter_Method,
                          Filter_value))
        }
      }
    },
    error = function(e) {
      return()
    })
  })

  # 相关性图像
  select_plot <- reactive({
    tryCatch({
      if (input$geneid_method == "ENSEMBL") {
        index <- "ensembl_gene_id"
      }
      if (input$geneid_method == "SYMBOL") {
        index <- "external_gene_name"
      }
      if (input$geneid_method == "ENTREZ") {
        index <- "entrezgene_id"
      }
      gene <- output_gene()
      if (length(which(duplicated(gene[[index]]))) > 0) {
        gene <-
          gene[-which(duplicated(gene[[index]])),]
      }

      click_ATAT <- req(input$ATAC_rows_selected)
      if (input$method == "1") {
        return(lzh_plot(
          gene,
          click_ATAT,
          "pearson",
          Filter_Method,
          Filter_value
        ))
      }
      if (input$method == "2") {
        return(lzh_plot(
          gene,
          click_ATAT,
          "spearman",
          Filter_Method,
          Filter_value
        ))
      }
      if (input$method == "3") {
        return(lzh_plot(
          gene,
          click_ATAT,
          "kendall",
          Filter_Method,
          Filter_value
        ))
      }
    },
    error = function(e) {
      #message("有限值的观察量不够", e)

      return(NULL)
    })
  })



  # 输出筛选基因
  output$RNA <- DT::renderDataTable(
    output_gene()[, c(1:6)],
    colnames = c('Ensembl ID', 'Symbol ID', 'Entrez ID', 'chrom', 'chromStart', 'chromEnd'),
    selection = "none",
    rownames = FALSE,
    extensions = c("Scroller", "RowReorder"),
    options = list(
      dom = "t",
      rowReorder = TRUE,
      deferRender = TRUE,
      scrollY = 150,
      scroller = TRUE,
      scrollX = TRUE,
      searchHighlight = TRUE,
      orderClasses = TRUE,
      autoWidth = F,
      fixedColumns = TRUE
    )
  )



  observe({
    if (is.null(select_ATAC())) {
      output$ATAC <- DT::renderDataTable(NULL)
    } else{
      output$ATAC <-
        DT::renderDataTable({
          df <- select_ATAC()[, c(-4:-(ncol(select_ATAC()) - 3))]
          # Create 19 breaks and 20 rgb color values ranging from white to blue
          brks1 <-
            quantile(df[, 6],
                     probs = seq(.05, .95, .05),
                     na.rm = TRUE)
          # clrs <- round(seq(0, 100, length.out = length(brks) + 1), 0) %>%
          #   {paste0("rgb(", .,",", .,",220)")}
          DT::datatable(
            df,
            selection = "single",
            extensions = c("Scroller", "RowReorder"),
            option = list(
              rowReorder = TRUE,
              deferRender = TRUE,
              scrollY = 295,
              scroller = TRUE,
              scrollX = TRUE,
              searchHighlight = TRUE,
              orderClasses = TRUE,
              autoWidth = F,
              fixedColumns = TRUE
            )
          ) %>%
            formatStyle('rho',background=color_from_middle(brks1,'lightpink','lightblue'))
        })
    }
  })

  # 输出图像
  output$displot <- renderPlotly({
    if (is.null(select_plot()))
      return()
    select_plot()
  })



  # 模块二 ------------------------------------------------------------------
  # 输出peak选择数据库
  observe({
    if (is.null(select_ATAC())) {
      output$ATAC2 <- DT::renderDataTable(NULL)
    } else{
      # 输出筛选基因
      output$RNA2 <- DT::renderDataTable(
        output_gene()[, c(1:6)],
        colnames = c('Ensembl ID', 'Symbol ID', 'Entrez ID', 'chrom', 'chromStart', 'chromEnd'),
        selection = "none",
        rownames = FALSE,
        extensions = c("Scroller", "RowReorder"),
        options = list(
          dom = "t",
          rowReorder = TRUE,
          deferRender = TRUE,
          scrollY = 150,
          scroller = TRUE,
          scrollX = TRUE,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          autoWidth = F,
          fixedColumns = TRUE
        )
      )


      output$ATAC2 <-
        DT::renderDataTable({
          df <- select_ATAC()[, c(-4:-(ncol(select_ATAC()) - 3))]
          # Create 19 breaks and 20 rgb color values ranging from white to blue
          brks1 <-
            quantile(
              df[, c(6)],
              probs = seq(.05, .95, .05),
              na.rm = TRUE
            )
          # clrs <- round(seq(0, 100, length.out = length(brks) + 1), 0) %>%
          #   {paste0("rgb(", .,",", .,",220)")}
          DT::datatable(
            df,
            selection = "single",
            # extensions = c("Scroller", "RowReorder"),
            option = list(
              pageLength = 10,
              pageWidth = 10,
              # autoWidth = F,
              searchHighlight = TRUE,
              # columnDefs = list(list(
              #   targets = 2, width = "210px"
              # )),
              scrollX = TRUE
            )
          ) %>%
            formatStyle('rho',background=color_from_middle(brks1,'lightpink','lightblue'))
        })
    }
  })



  # 轨道图
  select_trackplot <- reactive({
    tryCatch({
      peakfile <-
        select_ATAC()[, c(-(ncol(select_ATAC()) - 2):-ncol(select_ATAC()))] %>% data.frame()
      select_peak <- req(input$ATAC2_rows_selected)
      Species <- input$Species
      return(trackplot(peakfile, select_peak, Species))
    })
  })

  # 箱线图
  select_boxplot <- reactive({
    gene <- output_gene()
    if (input$geneid_method == "ENSEMBL") {
      index <- "ensembl_gene_id"
    }
    if (input$geneid_method == "SYMBOL") {
      index <- "external_gene_name"
    }
    if (input$geneid_method == "ENTREZ") {
      index <- "entrezgene_id"
    }
    if (length(which(duplicated(gene[[index]]))) > 0) {
      gene <-
        gene[-which(duplicated(gene[[index]])),]
    }
    peakfile <-
      select_ATAC()[, c(-(ncol(select_ATAC()) - 2):-ncol(select_ATAC()))]
    select_peak <- req(input$ATAC2_rows_selected)
    b <- box_plot(peakfile, gene, select_peak)
    return(b)
  })

  output$displot2 <- renderPlot({
    select_trackplot()
  })

  # 输出箱线图
  output$displot3 <- renderPlot({
    select_boxplot()
  })


  # 模块三 ------------------------------------------------------------------
  output$RNA3 <- DT::renderDataTable(
    output_gene()[, c(1:6)],
    colnames = c('Ensembl ID', 'Symbol ID', 'Entrez ID', 'chrom', 'chromStart', 'chromEnd'),
    selection = "none",
    rownames = FALSE,
    extensions = c("Scroller", "RowReorder"),
    options = list(
      dom = "t",
      rowReorder = TRUE,
      deferRender = TRUE,
      scrollY = 150,
      scroller = TRUE,
      scrollX = TRUE,
      searchHighlight = TRUE,
      orderClasses = TRUE,
      autoWidth = F,
      fixedColumns = TRUE
    )
  )


  observeEvent(input$Annote, {
    if (nrow(select_ATAC()) == 0) {
      sendSweetAlert(
        session = session,
        title = "ERROR",
        text = "This gene didn't find a peak with high correlation and couldn't be annotated!",
        type = "error"
      )
      return()
    }
    if (nrow(select_ATAC()) > 0) {
      progressSweetAlert(
        session = session,
        id = "simulationProgress1",
        title = "Commenting...",
        display_pct = TRUE,
        value = 20
      )

      peakfile <-
        select_ATAC()[, c(-(ncol(select_ATAC()) - 2):-ncol(select_ATAC()))]
      # select_peak <- req(input$ATAC2_rows_selected)
      gr <-
        GenomicRanges::makeGRangesFromDataFrame(peakfile, ignore.strand = TRUE)
      if (input$Species == "1") {
        txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene::TxDb.Hsapiens.UCSC.hg38.knownGene
        annoDb <- "org.Hs.eg.db"
      } else{
        txdb <- TxDb.Mmusculus.UCSC.mm10.knownGene::TxDb.Mmusculus.UCSC.mm10.knownGene
        annoDb <- "org.Mm.eg.db"
      }

      peakAnno <<- ChIPseeker::annotatePeak(
        gr,
        tssRegion = c(-3000, 3000),
        TxDb = txdb,
        annoDb = annoDb
      )
      Annotation_table <<- data.frame(peakAnno)

      updateProgressBar(
        session = session,
        id = "simulationProgress1",
        title = "Commenting...",
        value = 50
      )


      p <- ChIPseeker::upsetplot(peakAnno, vennpie = TRUE)

      output$Peak_Annotation <-
        DT::renderDataTable(
          Annotation_table,
          selection = "none",
          extensions = c("Scroller", "RowReorder"),
          option = list(
            rowReorder = TRUE,
            deferRender = TRUE,
            scrollY = 260,
            scroller = TRUE,
            scrollX = TRUE,
            searchHighlight = TRUE,
            orderClasses = TRUE,
            autoWidth = F
          )
        )

      output$displot4 <- renderPlot({
        p
      })


      updateProgressBar(
        session = session,
        id = "simulationProgress1",
        title = "Commenting...",
        value = 100
      )

      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title = "DONE",
        text = "The annotation is complete!",
        type = "success"
      )
    }
  })


  # 模块四 --------------------------------------------------------------------
  output$RNA4 <- DT::renderDataTable(
    output_gene()[, c(1:6)],
    colnames = c('Ensembl ID', 'Symbol ID', 'Entrez ID', 'chrom', 'chromStart', 'chromEnd'),
    selection = "none",
    rownames = FALSE,
    extensions = c("Scroller", "RowReorder"),
    options = list(
      dom = "t",
      rowReorder = TRUE,
      deferRender = TRUE,
      scrollY = 150,
      scroller = TRUE,
      scrollX = TRUE,
      searchHighlight = TRUE,
      orderClasses = TRUE,
      autoWidth = F,
      fixedColumns = TRUE
    )
  )

  observe({
    if (is.null(select_ATAC())) {
      output$ATAC3 <- DT::renderDataTable(NULL)
    } else{
      output$ATAC3 <-
        DT::renderDataTable({
          df <- select_ATAC()[, c(-4:-(ncol(select_ATAC()) - 3))]
          # Create 19 breaks and 20 rgb color values ranging from white to blue
          brks1 <-
            quantile(
              df[, c(6)],
              probs = seq(.05, .95, .05),
              na.rm = TRUE
            )
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
            formatStyle('rho',background=color_from_middle(brks1,'lightpink','lightblue'))
        })
    }
  })



  # motif分析
  select_motif <- reactive({
    peakfile <-
      select_ATAC()[, c(-(ncol(select_ATAC()) - 2):-ncol(select_ATAC()))]
    select_peak <- req(input$ATAC3_rows_selected)
    Species <- input$Species
    return(motif_analysis(peakfile, select_peak, Species))

  })

  # seqlogo图
  select_seqlogo <- reactive({
    motif <- select_motif()
    select_row <- req(input$Motif_rows_selected)
    return(seqLogo_plot(motif, select_row))
  })


  output$Motif <-
    DT::renderDataTable({
      df <- select_motif()
      # Create 19 breaks and 20 rgb color values ranging from white to blue
      brks1 <-
        quantile(df$score,
                 probs = seq(.05, .95, .05),
                 na.rm = TRUE)

      # clrs <- round(seq(0, 100, length.out = length(brks) + 1), 0) %>%
      #   {paste0("rgb(", .,",", .,",220)")}
      DT::datatable(
        df,
        selection = "single",
        extensions = c("Scroller", "RowReorder"),
        option = list(
          rowReorder = TRUE,
          deferRender = TRUE,
          scrollY = 295,
          scroller = TRUE,
          scrollX = TRUE,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          autoWidth = F
        )
      ) %>%
        formatStyle(names(df)[8],background=color_from_middle(brks1,'red','lightblue'))
    })
  # 输出seqlogo图
  output$displot5 <- renderPlot({
    select_seqlogo()
  })


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
      }

      else if (length(intersect(gene.list, RNA_count()[, index])) == 0) {
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
            # message("发生了错误: ", conditionMessage(e))
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
            # message("发生了错误: ", conditionMessage(e))
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
        genome <- "hg38"
      }
      if (input$Species == "2") {
        PFMatrixList <- Mus.PFMatrixList
        pwm_library_dt <- Mus.pwm_library_dt
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
        peaks <- GenomicRanges::GRanges(
          seqnames = c(peaks$chrom),
          ranges = IRanges::IRanges(
            start = peaks$chromStart,
            end = peaks$chromEnd
          )
        )

        # Get motif positions within peaks for example motifs in peaks
        motif_ix <- motifmatchr::matchMotifs(PFMatrixList,
                                             peaks,
                                             genome = genome,
                                             out = "positions") %>% data.frame()

        motif[[i]] <- pwm_library_dt[motif_ix$group, ]
        motif[[i]] <- cbind(motif[[i]], motif_ix)
        motif[[i]] <- unique(motif[[i]]$name)

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
            # message("发生了错误: ", conditionMessage(e))
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
            # message("发生了错误: ", conditionMessage(e))
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
              3,
              radioButtons(
                inputId = "extTable4",
                label = helpText("Output Format"),
                choices = c("CSV" = "csv", "TXT" = "txt"),
                inline = T
              )
            ),
            column(3,
                   div(
                     downloadButton("Download_vis", "Download"),
                     shiny::tags$style(
                       "#Download_vis {background-color: white; color: black; margin-left: 0%;margin-top: 25px;box-shadow: inset 0px 1px 2.5px #888888;}"
                     ),
                   ))
            # column(
            #   4,
            #   do.call(actionBttn, c(
            #     list(
            #       inputId = "submit8",
            #       label = "Go Next",
            #       icon = icon("play")
            #     ),
            #     actionBttnParams
            #   )),
            #   shiny::tags$style(
            #     "#submit8 {margin-top: 10%;box-shadow: 0px 2px 5px #888888;}"
            #   ),
            # )
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
            colnames = c("Gene","TF","p_value","FDR","rho"),
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
          )  %>%
            formatStyle('rho',background=color_from_middle(brks1,'lightpink','lightblue'))
        })
    }
  })

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

      #if(nrow(Gene.TF.frame[rowSums(is.na(Gene.TF.frame)) == 4,]) > 0){
      Gene.TF.frame.filter <-
        rbind(Gene.TF.frame.filter, Gene.TF.frame[rowSums(is.na(Gene.TF.frame)) == 6, c("gene", "TF", "p_value", "FDR", "rho")], use.names =
                FALSE)
      #}
      Gene.TF.frame.filter <-
        Gene.TF.frame.filter[-which(Gene.TF.frame.filter$gene == FALSE),]

      # Gene.TF.frame.filter <- Gene.TF.frame.filter[,c("gene","TF",input$TF_filter_method)]
      negative.TF <-
        Gene.TF.frame.filter[Gene.TF.frame.filter$rho < 0,]$TF
      positive.TF <-
        Gene.TF.frame.filter[Gene.TF.frame.filter$rho > 0,]$TF

      Gene <- Gene.TF.frame.filter %>%
        distinct(gene) %>%
        dplyr::rename(label = gene)

      #目的地去重
      TFs <- Gene.TF.frame.filter %>%
        distinct(TF) %>%
        dplyr::rename(label = TF)

      ## 合并数据并添加一列索引
      nodes <- rbind(Gene, TFs)
      nodes <- nodes %>%
        mutate(id = 1:nrow(nodes)) %>%
        dplyr::select(id, everything())
      nodes <- nodes[!duplicated(nodes$label),]
      #id has to be the same like from and to columns in edges
      nodes$id <- nodes$label
      edges <- Gene.TF.frame.filter
      colnames(edges)[c(1:2)] <- c("from", "to")

      TF_filter_method <- input$TF_filter_method
      edges$value <- abs(edges[["rho"]])
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
               "lightblue")
      tryCatch({
        if (length(which(rowSums(is.na(nodes)) > 0)) > 0) {
          nodes <- nodes[-which(rowSums(is.na(nodes)) > 0),]
        }
        RNA_count <- RNA_count()
        RNA_count <-
          RNA_count[-which(duplicated(RNA_count[[index]])),]
        nodes$value <-
          apply(RNA_count[RNA_count[[index]] %in% nodes$label, c(-1:-6)], 1, mean)
      }, error = function(e) {
        print(e)
        nodes$value <- 1
      })

      if (length(which(rowSums(is.na(edges)) > 0))) {
        edges <- edges[-which(rowSums(is.na(edges)) > 0),]
      }
      edges$color <-
        ifelse(edges$to %in% positive.TF, "#FF8C00", "lightgreen")

      network <- visNetwork(nodes, edges) %>%
        visNodes(size = 10) %>%
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
                label = helpText("Output Format"),
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
                  label = "Continue",
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
        dplyr::select(Gene.TF.frame.filter,
                      c("gene", "TF", "p_value", "FDR", "rho"))

      Gene.TF.frame.filter <-
        rbind(Gene.TF.frame.filter, Gene.TF.frame[rowSums(is.na(Gene.TF.frame)) == 6, c("gene", "TF", "p_value", "FDR", "rho")], use.names =
                FALSE)
      negative.TF <-
        Gene.TF.frame.filter[Gene.TF.frame.filter$rho < 0,]$TF
      positive.TF <-
        Gene.TF.frame.filter[Gene.TF.frame.filter$rho > 0,]$TF
      Gene <- Gene.TF.frame.filter %>%
        distinct(gene) %>%
        dplyr::rename(label = gene)
      TFs <- Gene.TF.frame.filter %>%
        distinct(TF) %>%
        dplyr::rename(label = TF)
      nodes <- rbind(Gene, TFs)
      nodes <- nodes %>%
        mutate(id = 1:nrow(nodes)) %>%
        dplyr::select(id, everything())
      nodes <- nodes[!duplicated(nodes$label),]
      nodes$id <- nodes$label
      edges <- Gene.TF.frame.filter
      colnames(edges)[c(1:2)] <- c("from", "to")
      edges$value <- abs(edges[["rho"]])
      graph <- graph_from_data_frame(edges, directed = FALSE)
      cluster <- cluster_louvain(graph)
      cluster_df <- data.frame(as.list(membership(cluster)),check.names = FALSE)
      cluster_df <- as.data.frame(t(cluster_df))
      cluster_df$label <- as.character(rownames(cluster_df))
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
               "lightblue")
      tryCatch({
        nodes <- na.omit(nodes)
        RNA_count <- RNA_count()
        RNA_count <-
          RNA_count[-which(duplicated(RNA_count[[index]])),]
        nodes$value <-
          apply(RNA_count[RNA_count[[index]] %in% nodes$label, c(-1:-6)], 1, mean)
      }, error = function(e) {
        print(e)
        nodes$value <- 1
      })

      if (length(which(rowSums(is.na(edges)) > 0))) {
        edges <- edges[-which(rowSums(is.na(edges)) > 0),]
      }
      edges$color <-
        ifelse(edges$to %in% positive.TF, "#FF8C00", "lightgreen")

      network <- visNetwork(nodes, edges) %>%
        visNodes(size = 10) %>%
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
    }, error = function(e) {
      print(e)

      tryCatch({
        Gene.TF.frame.filter <- input_networkdata()
        colnames(Gene.TF.frame.filter)[1:2] <- c("Target", "Source")

        if (length(which(Gene.TF.frame.filter$Source == FALSE)) > 0) {
          Gene.TF.frame.filter <-
            Gene.TF.frame.filter[-which(Gene.TF.frame.filter$Source == FALSE),]
        }


        Gene <- Gene.TF.frame.filter %>%
          distinct(Target) %>%
          dplyr::rename(label = Target)

        TFs <- Gene.TF.frame.filter %>%
          distinct(Source) %>%
          dplyr::rename(label = Source)

        nodes <- rbind(Gene, TFs)
        nodes <- nodes %>%
          mutate(id = 1:nrow(nodes)) %>%
          dplyr::select(id, everything())
        nodes <- nodes[!duplicated(nodes$label),]
        nodes$id <- nodes$label

        edges <- Gene.TF.frame.filter
        colnames(edges)[c(1:2)] <- c("from", "to")

        graph <- graph_from_data_frame(edges, directed = FALSE)
        cluster <- cluster_louvain(graph)
        cluster_df <- data.frame(as.list(membership(cluster)))
        cluster_df <- as.data.frame(t(cluster_df))
        cluster_df$label <- as.character(rownames(cluster_df))
        nodes$label <- as.character(nodes$label)
        nodes <- left_join(nodes, cluster_df, by = "label")
        colnames(nodes)[3] <- "group"

        nodes$color <-
          ifelse(nodes$id %in% Gene.TF.frame.filter$Source,
                 "red",
                 "lightblue")

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

  # 模块六 ------------------------------------------------------------------
  observeEvent(input$submit7, {
    progressSweetAlert(
      session = session,
      id = "simulationProgress6",
      title = "GO enrichment analysis...",
      display_pct = TRUE,
      value = 20
    )

    gene_list <- input$gene_list_six
    gene_list <- unlist(strsplit(gene_list, "\n"))

    if (input$six_genelist_idtype == "1") {
      index <- "ensembl_gene_id"
    }
    if (input$six_genelist_idtype == "2") {
      index <- "external_gene_name"
    }
    if (input$six_genelist_idtype == "3") {
      index <- "entrezgene_id"
    }
    RNA <- RNA_count()

    if (input$Species == "1") {
      if (input$six_genelist_idtype != "3") {
        gene <- RNA[RNA[[index]] %in% gene_list, 3]
      }
      OrgDb <- "org.Hs.eg.db"
      organism <- 'hsa'
    } else{
      if (input$six_genelist_idtype != "3") {
        gene <- RNA[RNA[[index]] %in% gene_list, 3]
      }
      OrgDb <- "org.Mm.eg.db"
      organism <- 'mmu'
    }

    go <<- enrichGO(
      gene = gene,
      OrgDb = OrgDb,
      ont = "ALL",
      pAdjustMethod = "BH",
      pvalueCutoff = input$six_p_value,
      qvalueCutoff = input$six_q_value,
      minGSSize = input$minGSSize,
      maxGSSize = input$maxGSSize,
      readable = T
    )

    updateProgressBar(
      session = session,
      id = "simulationProgress6",
      title = "KEGG enrichment analysis...",
      value = 50
    )

    kegg <<- enrichKEGG(
      gene = gene,
      organism = organism,
      pvalueCutoff = input$six_p_value,
      qvalueCutoff = input$six_q_value,
      minGSSize = input$minGSSize,
      maxGSSize = input$maxGSSize
    )

    output$Enrichment.Table <- renderUI({
      shinydashboard::tabBox(
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1",
        width = 9,
        tabPanel(
          tagList(icon("table"),"GO Enrichment Table"),
          dataTableOutput("GO.result"),
          fluidRow(
            column(
              3,
              radioButtons(
                inputId = "extTable5",
                label = helpText("Output Format"),
                choices = c("CSV" = "csv", "TXT" = "txt"),
                inline = T
              )
            ),
            column(
              9,
              downloadButton("Download_GO", "Download"),
              shiny::tags$style(
                "#Download_GO {background-color: white; color: black;margin-left:0%;margin-top:25px;box-shadow: inset 0px 1px 2.5px #888888;}"
              )
            )
          )
        ),

        tabPanel(
          tagList(icon("table"),"KEGG Enrichment Table"),
          dataTableOutput("KEGG.result"),
          fluidRow(
            column(
              3,
              radioButtons(
                inputId = "extTable6",
                label = helpText("Output Format"),
                choices = c("CSV" = "csv", "TXT" = "txt"),
                inline = T
              )
            ),
            column(
              3,
              downloadButton("Download_KEGG", "Download"),
              shiny::tags$style(
                "#Download_KEGG {background-color: white; color: black;margin-left:0%;margin-top:25px;box-shadow: inset 0px 1px 2.5px #888888;}"
              )
            )
          )
        )
      )
    })

    output$Enrichment.Plot <- renderUI({
      shinydashboard::tabBox(
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset2",
        width = 12,
        height = "auto",
        tabPanel(tagList(icon(
          "chart-column"
        ), "GO dotplot"),
        fluidRow(
          column(3,
                 fluidRow(column(
                   12,
                   radioButtons(
                     inputId = "extPlot6",
                     label = helpText("Output Format"),
                     choices = c(
                       "PNG" = "png",
                       "PDF" = "pdf",
                       "JPEG" = "jpeg"
                     ),
                     inline = T
                   )
                 )),
                 fluidRow(
                   column(
                     12,
                     downloadButton("plotDown6", "Download"),
                     shiny::tags$style(
                       "#plotDown6 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                     )
                   )
                 )),
          column(9,
                 plotOutput("GO.dotplot"))
        )),
        tabPanel(tagList(icon("chart-line"), "GO barplot"),
                 fluidRow(
                   column(3,
                          fluidRow(column(
                            12,
                            radioButtons(
                              inputId = "extPlot7",
                              label = helpText("Output Format"),
                              choices = c(
                                "PNG" = "png",
                                "PDF" = "pdf",
                                "JPEG" = "jpeg"
                              ),
                              inline = T
                            )
                          )),
                          fluidRow(
                            column(
                              12,
                              downloadButton("plotDown7", "Download"),
                              shiny::tags$style(
                                "#plotDown7 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                              )
                            )
                          )),
                   column(9,
                          plotOutput("GO.barplot"))
                 )),
        tabPanel(
          tagList(icon("chart-area"), "GO upsetplot"),
          fluidRow(column(
            3,
            fluidRow(column(
              12,
              radioButtons(
                inputId = "extPlot8",
                label = helpText("Output Format"),
                choices = c(
                  "PNG" = "png",
                  "PDF" = "pdf",
                  "JPEG" = "jpeg"
                ),
                inline = T
              )
            )),
            fluidRow(column(
              12,
              downloadButton("plotDown8", "Download"),
              shiny::tags$style(
                "#plotDown8 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
              )
            ))
          ),
          column(9,
                 plotOutput(
                   "GO.upsetplot"
                 )))
        ),
        tabPanel(
          tagList(icon("sitemap"), "GO cnetplot"),
          fluidRow(column(
            3,
            fluidRow(column(
              12,
              radioButtons(
                inputId = "extPlot13",
                label = helpText("Output Format"),
                choices = c(
                  "PNG" = "png",
                  "PDF" = "pdf",
                  "JPEG" = "jpeg"
                ),
                inline = T
              )
            )),
            fluidRow(column(
              12,
              downloadButton("plotDown13", "Download"),
              shiny::tags$style(
                "#plotDown13 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
              )
            ))
          ),
          column(9,
                 plotOutput(
                   "GO.cnetplot"
                 )))
        ),
        tabPanel(tagList(
          icon("object-group"), "GO wordcloud"
        ),
        fluidRow(
          column(12,
                 wordcloud2::wordcloud2Output("GO.wordcloud"))
        )),
        tabPanel(
          tagList(icon("chart-column"), "KEGG dotplot"),
          fluidRow(
            column(3,
                   fluidRow(column(
                     12,
                     radioButtons(
                       inputId = "extPlot10",
                       label = helpText("Output Format"),
                       choices = c(
                         "PNG" = "png",
                         "PDF" = "pdf",
                         "JPEG" = "jpeg"
                       ),
                       inline = T
                     )
                   )),
                   fluidRow(
                     column(
                       12,
                       downloadButton("plotDown10", "Download"),
                       shiny::tags$style(
                         "#plotDown10 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                       )
                     )
                   )),
            column(9,
                   plotOutput("KEGG.dotplot"))
          )
        ),
        tabPanel(
          tagList(icon("chart-line"), "KEGG barplot"),
          fluidRow(
            column(3,
                   fluidRow(column(
                     12,
                     radioButtons(
                       inputId = "extPlot11",
                       label = helpText("Output Format"),
                       choices = c(
                         "PNG" = "png",
                         "PDF" = "pdf",
                         "JPEG" = "jpeg"
                       ),
                       inline = T
                     )
                   )),
                   fluidRow(
                     column(
                       12,
                       downloadButton("plotDown11", "Download"),
                       shiny::tags$style(
                         "#plotDown11 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
                       )
                     )
                   )),
            column(9,
                   plotOutput("KEGG.barplot"))
          )
        ),
        tabPanel(
          tagList(icon("chart-area"), "KEGG upsetplot"),
          fluidRow(column(
            3,
            fluidRow(column(
              12,
              radioButtons(
                inputId = "extPlot12",
                label = helpText("Output Format"),
                choices = c(
                  "PNG" = "png",
                  "PDF" = "pdf",
                  "JPEG" = "jpeg"
                ),
                inline = T
              )
            )),
            fluidRow(column(
              12,
              downloadButton("plotDown12", "Download"),
              shiny::tags$style(
                "#plotDown12 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
              )
            ))
          ),
          column(9,
                 plotOutput(
                   "KEGG.upsetplot"
                 )))
        ),
        tabPanel(
          tagList(icon("sitemap"), "KEGG cnetplot"),
          fluidRow(column(
            3,
            fluidRow(column(
              12,
              radioButtons(
                inputId = "extPlot14",
                label = helpText("Output Format"),
                choices = c(
                  "PNG" = "png",
                  "PDF" = "pdf",
                  "JPEG" = "jpeg"
                ),
                inline = T
              )
            )),
            fluidRow(column(
              12,
              downloadButton("plotDown14", "Download"),
              shiny::tags$style(
                "#plotDown14 {background-color: white; color: black;margin-left:10%;margin-top: 10%;box-shadow: inset 0px 1px 2.5px #888888;}"
              )
            ))
          ),
          column(9,
                 plotOutput(
                   "KEGG.cnetplot"
                 )))
        ),
        tabPanel(tagList(
          icon("object-group"), "KEGG wordcloud"
        ),
        fluidRow(
          column(12,
                 wordcloud2::wordcloud2Output("KEGG.wordcloud"))
        ))

      )
    })

    tryCatch({
      output$GO.result <- DT::renderDataTable(
        go@result,
        extensions = c("Scroller", "RowReorder"),
        server = FALSE,
        selection = "none",
        option = list(
          rowReorder = TRUE,
          deferRender = TRUE,
          scrollY = 400,
          scroller = TRUE,
          scrollX = TRUE,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          autoWidth = F
        )
      )

      wcdf.GO <- read.table(text = go$GeneRatio, sep = "/")[1]
      wcdf.GO$word <- go[, 3]
      names(wcdf.GO)[1] <- "freq"
      wcdf.GO <- dplyr::select(wcdf.GO, c(2, 1))


      output$GO.dotplot <- renderPlot({
        # plotly::ggplotly(
          dotplot(go) + theme(axis.text.y = element_text(size = 10)) + scale_y_discrete(
            labels = function(x)
              str_wrap(x, width = 100)
          )
        # )
      })

      output$GO.barplot <- renderPlot({
        # plotly::ggplotly(
          barplot(go) + theme(axis.text.y = element_text(size = 10)) + scale_y_discrete(
            labels = function(x)
              str_wrap(x, width = 100)
          )
        # )
      })

      output$GO.upsetplot <- renderPlot({
        upsetplot(go)
      })

      output$GO.cnetplot <- renderPlot({
        cnetplot(go)
      })

      output$GO.wordcloud <- wordcloud2::renderWordcloud2({
        wordcloud2(wcdf.GO, size = 1)
      })
    }, error = function(e) {
      output$GO.result <- DT::renderDataTable(return())



      output$GO.dotplot <- renderPlot({
        return()
      })

      output$GO.barplot <- renderPlot({
        return()
      })

      output$GO.upsetplot <- renderPlot({
        return()
      })

      output$GO.wordcloud <- wordcloud2::renderWordcloud2({
        return()
      })

      output$GO.cnetplot <- renderPlot({
        return()
      })
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title = "ERROR",
        text = "GO did not enrich to the result!",
        type = "error"
      )
    })

    tryCatch({
      wcdf.KEGG <- read.table(text = kegg$GeneRatio, sep = "/")[1]
      wcdf.KEGG$word <- kegg[, 2]
      names(wcdf.KEGG)[1] <- "freq"
      wcdf.KEGG <- dplyr::select(wcdf.KEGG, c(2, 1))
      output$KEGG.result <- DT::renderDataTable(
        kegg@result,
        extensions = c("Scroller", "RowReorder"),
        server = FALSE,
        selection = "none",
        option = list(
          rowReorder = TRUE,
          deferRender = TRUE,
          scrollY = 400,
          scroller = TRUE,
          scrollX = TRUE,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          autoWidth = F
        )
      )
      output$KEGG.dotplot <- renderPlot({
        # plotly::ggplotly(
          dotplot(kegg) + theme(axis.text.y = element_text(size = 10)) + scale_y_discrete(
            labels = function(x)
              str_wrap(x, width = 100)
          )
        # )
      })

      output$KEGG.barplot <- renderPlot({
        # plotly::ggplotly(
          barplot(kegg) + theme(axis.text.y = element_text(size = 10)) + scale_y_discrete(
            labels = function(x)
              str_wrap(x, width = 100)
          )
        # )
      })

      output$KEGG.upsetplot <- renderPlot({
        upsetplot(kegg)
      })

      output$KEGG.cnetplot <- renderPlot({
        cnetplot(kegg)
      })

      output$KEGG.wordcloud <- wordcloud2::renderWordcloud2({
        wordcloud2(wcdf.KEGG, size = 1)
      })
      updateProgressBar(
        session = session,
        id = "simulationProgress6",
        title = "KEGG enrichment analysis...",
        value = 100
      )

      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title = "DONE",
        text = "The annotation is complete!",
        type = "success"
      )
    }, error = function(e) {
      output$KEGG.result <- DT::renderDataTable(return())
      output$KEGG.dotplot <- renderPlotly({
        return()
      })

      output$KEGG.barplot <- renderPlotly({
        return()
      })

      output$KEGG.upsetplot <- renderPlot({
        return()
      })

      output$KEGG.cnetplot <- renderPlot({
        return()
      })

      output$KEGG.wordcloud <- wordcloud2::renderWordcloud2({
        return()
      })
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title = "ERROR",
        text = "KEGG did not enrich to the result!",
        type = "error"
      )
    })
  })
  # 下载 --------------------------------------------------------------------
  output$tableDown <- downloadHandler(
    filename = function() {
      paste0(output_gene()[, 1], ".", input$extTable)
    },
    content = function(file) {
      if (input$extTable == "csv") {
        write.csv(select_ATAC(),
                  file,
                  row.names = F,
                  quote = F)
      } else {
        write.table(
          select_ATAC(),
          file,
          sep = "\t",
          col.names = T,
          row.names = F,
          quote = F
        )
      }
    }
  )
  output$plotDown <- downloadHandler(
    filename = function() {
      paste0(output_gene()[, 1],
             "_",
             input$ATAC_rows_selected,
             ".",
             input$extPlot)
    },
    content = function(file) {
      if (input$extPlot == "pdf") {
        pdf(file)
      } else if (input$extPlot == "png") {
        png(file)
      } else {
        jpeg(file)
      }
      plot(fig)
      dev.off()
    }
  )
  output$plotDown2 <- downloadHandler(
    filename = function() {
      paste0(output_gene()[, 1],
             "_",
             input$ATAC2_rows_selected,
             "trackplot.",
             input$extPlot2)
    },
    content = function(file) {
      if (input$extPlot2 == "pdf") {
        pdf(file)
      } else if (input$extPlot2 == "png") {
        png(file)
      } else {
        jpeg(file)
      }
      peakfile <-
        select_ATAC()[, c(-(ncol(select_ATAC()) - 2):-ncol(select_ATAC()))]
      select_peak <- req(input$ATAC2_rows_selected)
      df <- peakfile[, c(-1:-3)]
      t_df <- t(df)
      dt_df <- data.frame(t_df)
      sort_df <- dt_df[order(dt_df[, 1]), , drop = FALSE]
      v1 <- sort_df[c(1:(nrow(sort_df) %/% 5)), , drop = FALSE]
      v2 <-
        sort_df[c(((nrow(sort_df) %/% 5) + 1):(2 * (nrow(sort_df) %/% 5))), , drop = FALSE]
      v3 <-
        sort_df[c((2 * (nrow(sort_df) %/% 5) + 1):(3 * (nrow(sort_df) %/% 5))), , drop = FALSE]
      v4 <-
        sort_df[c((3 * (nrow(sort_df) %/% 5) + 1):(4 * (nrow(sort_df) %/% 5))), , drop = FALSE]
      v5 <-
        sort_df[c((4 * (nrow(sort_df) %/% 5) + 1):nrow(sort_df)), , drop = FALSE]
      m1 <- colMeans(v1)
      m2 <- colMeans(v2)
      m3 <- colMeans(v3)
      m4 <- colMeans(v4)
      m5 <- colMeans(v5)
      data <-
        data.frame(
          group1 = m1,
          group2 = m2,
          group3 = m3,
          group4 = m4,
          group5 = m5
        )
      gr <-
        GenomicRanges::makeGRangesFromDataFrame(peakfile, ignore.strand = TRUE)
      GenomicRanges::values(gr) <- data

      ax <- Gviz::GenomeAxisTrack()

      tracks_list <- list()
      for (i in 1:length(names(elementMetadata(gr)))) {
        track_name <- paste("track", i, sep = "")
        tracks_list[[i]] <- Gviz::DataTrack(
          gr[, names(elementMetadata(gr))[i]],
          genome = "hg38",
          name = names(elementMetadata(gr))[i],
          type = "histogram",
          ylim = c(-1, 10)
        )
      }

      # genome
      gen <- genome(tracks_list[[i]])
      chr <- as.character(unique(seqnames(tracks_list[[i]])))
      peak <- peakfile[select_peak,]
      tryCatch({
        itrack <- Gviz::IdeogramTrack(genome = gen, chromosome = chr)
        ht <- Gviz::HighlightTrack(
          tracks_list,
          start = peak$chromStart,
          width = as.numeric(peak$chromEnd - peak$chromStart),
          chromosome = substring(peak[, 1], 4)
        )
        Gviz::plotTracks(list(ht, ax, itrack), type = "histogram", col = NULL)
      },
      error = function(e) {
        ht <- Gviz::HighlightTrack(
          tracks_list,
          start = peak$chromStart,
          width = as.numeric(peak$chromEnd - peak$chromStart),
          chromosome = substring(peak[, 1], 4)
        )
        Gviz::plotTracks(list(ht, ax), type = "histogram", col = NULL)
      })
      dev.off()
    }
  )
  output$plotDown3 <- downloadHandler(
    filename = function() {
      paste0(output_gene()[, 1],
             "_",
             input$ATAC_rows_selected,
             "boxplot.",
             input$extPlot3)
    },
    content = function(file) {
      if (input$extPlot3 == "pdf") {
        pdf(file)
      } else if (input$extPlot3 == "png") {
        png(file)
      } else {
        jpeg(file)
      }
      plot(b)
      dev.off()
    }
  )
  output$plotDown4 <- downloadHandler(
    filename = function() {
      paste0(output_gene()[, 1],
             "_",
             input$ATAC_rows_selected,
             "upsetplot.",
             input$extPlot4)
    },
    content = function(file) {
      if (input$extPlot4 == "pdf") {
        pdf(file)
      } else if (input$extPlot4 == "png") {
        png(file)
      } else {
        jpeg(file)
      }
      plot(ChIPseeker::upsetplot(peakAnno, vennpie = TRUE))
      dev.off()
    }
  )
  output$Download_Annotation <- downloadHandler(
    filename = function() {
      paste0(output_gene()[, 1], "_PeakAnnotation.", input$extTable2)
    },
    content = function(file) {
      if (input$extTable2 == "csv") {
        write.csv(Annotation_table,
                  row.names = F,
                  quote = F,
                  file)
      } else {
        write.table(
          Annotation_table,
          file,
          sep = "\t",
          col.names = T,
          row.names = F,
          quote = F
        )
      }
    }
  )
  output$Download_Motif <- downloadHandler(
    filename = function() {
      paste0(output_gene()[, 1], "_Motif.", input$extTable3)
    },
    content = function(file) {
      if (input$extTable3 == "csv") {
        write.csv(select_motif(),
                  row.names = F,
                  quote = F,
                  file)
      } else {
        write.table(
          select_motif(),
          file,
          sep = "\t",
          col.names = T,
          row.names = F,
          quote = F
        )
      }
    }
  )
  output$plotDown5 <- downloadHandler(
    filename = function() {
      paste0(output_gene()[, 1],
             "_",
             input$Motif_rows_selected,
             "seqLogo.",
             input$extPlot5)
    },
    content = function(file) {
      if (input$extPlot5 == "pdf") {
        pdf(file)
      } else if (input$extPlot5 == "png") {
        png(file)
      } else {
        jpeg(file)
      }
      motif <- select_motif()
      select_row <- req(input$Motif_rows_selected)
      select_motif <- motif[select_row,]
      m <-
        TFBSTools::getMatrixByID('JASPAR2022.sqlite', select_motif$ID)
      seqLogo(toICM(m))
      dev.off()
    }
  )

  output$plotDown6 <- downloadHandler(
    filename = function() {
      paste0("GO_dotplot.",
             input$extPlot6)
    },
    content = function(file) {
      if (input$extPlot6 == "pdf") {
        pdf(file)
      } else if (input$extPlot6 == "png") {
        png(file)
      } else {
        jpeg(file)
      }
      plot(
        dotplot(go) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
          labels = function(x)
            str_wrap(x, width = 30)
        )
      )
      dev.off()
    }
  )

  output$plotDown7 <- downloadHandler(
    filename = function() {
      paste0("GO_barplot.",
             input$extPlot7)
    },
    content = function(file) {
      if (input$extPlot7 == "pdf") {
        pdf(file)
      } else if (input$extPlot7 == "png") {
        png(file)
      } else {
        jpeg(file)
      }
      plot(
        barplot(go) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
          labels = function(x)
            str_wrap(x, width = 30)
        )
      )
      dev.off()
    }
  )

  output$plotDown8 <- downloadHandler(
    filename = function() {
      paste0("GO_upsetplot.",
             input$extPlot8)
    },
    content = function(file) {
      if (input$extPlot8 == "pdf") {
        pdf(file)
      } else if (input$extPlot8 == "png") {
        png(file)
      } else {
        jpeg(file)
      }
      plot(upsetplot(go))
      dev.off()
    }
  )

  output$plotDown13 <- downloadHandler(
    filename = function() {
      paste0("GO_cnetplot.",
             input$extPlot13)
    },
    content = function(file) {
      if (input$extPlot13 == "pdf") {
        pdf(file)
      } else if (input$extPlot13 == "png") {
        png(file)
      } else {
        jpeg(file)
      }
      plot(cnetplot(go))
      dev.off()
    }
  )

  output$plotDown10 <- downloadHandler(
    filename = function() {
      paste0("KEGG_dotplot.",
             input$extPlot10)
    },
    content = function(file) {
      if (input$extPlot10 == "pdf") {
        pdf(file)
      } else if (input$extPlot10 == "png") {
        png(file)
      } else {
        jpeg(file)
      }
      plot(
        dotplot(kegg) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
          labels = function(x)
            str_wrap(x, width = 30)
        )
      )
      dev.off()
    }
  )

  output$plotDown11 <- downloadHandler(
    filename = function() {
      paste0("KEGG_barplot.",
             input$extPlot11)
    },
    content = function(file) {
      if (input$extPlot11 == "pdf") {
        pdf(file)
      } else if (input$extPlot11 == "png") {
        png(file)
      } else {
        jpeg(file)
      }
      plot(
        barplot(kegg) + theme(axis.text.y = element_text(size = 8)) + scale_y_discrete(
          labels = function(x)
            str_wrap(x, width = 30)
        )
      )
      dev.off()
    }
  )

  output$plotDown12 <- downloadHandler(
    filename = function() {
      paste0("KEGG_upsetplot.",
             input$extPlot12)
    },
    content = function(file) {
      if (input$extPlot12 == "pdf") {
        pdf(file)
      } else if (input$extPlot12 == "png") {
        png(file)
      } else {
        jpeg(file)
      }
      plot(upsetplot(kegg))
      dev.off()
    }
  )

  output$plotDown14 <- downloadHandler(
    filename = function() {
      paste0("KEGG_cnetplot.",
             input$extPlot14)
    },
    content = function(file) {
      if (input$extPlot14 == "pdf") {
        pdf(file)
      } else if (input$extPlot14 == "png") {
        png(file)
      } else {
        jpeg(file)
      }
      plot(cnetplot(kegg))
      dev.off()
    }
  )
  output$Download_GO <- downloadHandler(
    filename = function() {
      paste0("GO_result.", input$extTable5)
    },
    content = function(file) {
      if (input$extTable5 == "csv") {
        write.csv(go@result,
                  row.names = F,
                  quote = F,
                  file)
      } else {
        write.table(
          go@result,
          file,
          sep = "\t",
          col.names = T,
          row.names = F,
          quote = F
        )
      }
    }
  )
  output$Download_KEGG <- downloadHandler(
    filename = function() {
      paste0("KEGG_result.", input$extTable6)
    },
    content = function(file) {
      if (input$extTable6 == "csv") {
        write.csv(kegg@result,
                  row.names = F,
                  quote = F,
                  file)
      } else {
        write.table(
          kegg@result,
          file,
          sep = "\t",
          col.names = T,
          row.names = F,
          quote = F
        )
      }
    }
  )

  output$Download_vis <- downloadHandler(
    filename = function() {
      paste0("Network_table.", input$extTable4)
    },
    content = function(file) {
      if (input$extTable4 == "csv") {
        write.csv(BuildNetwork(),
                  row.names = F,
                  quote = F,
                  file)
      } else {
        write.table(
          BuildNetwork(),
          file,
          sep = "\t",
          col.names = T,
          row.names = F,
          quote = F
        )
      }
    }
  )
}
