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
      # 使用当前列名作为by.x参数的值，第二个数据框的第一列的列名作为by.y参数的值
      tryCatch({
        # print(RNA[1,1])
        # a <- substr(RNA[1,1],1,4)
        # if(a == "ENSG" | a == "ENSM"){
        #   RNA[,1] <- str_split(RNA[,1], "\\.", simplify = T)[, 1]
        # }
        merge_df <-
          merge(
            gene.positions,
            RNA,
            by.x = col_name,
            by.y = colnames(RNA)[1],
            all.x = F,
            all.y = F
          )
        # a <- summary(merged_df)
        if (nrow(merge_df) > 0) {
          break
        }
        # print(nrow(merged_df))
      }, error = function(e) {
        # print(e)
      })
    }
    merge_df <- as.data.frame(merge_df)
    #colnames(merge_df)[c(1:6)] <-
    #c(
    #"ensembl_gene_id",
    #"external_gene_name",
    #"entrezgene_id",
    # "chromosome_name",
    # "start_position",
    #  "end_position"
    #)
    #merge_df <- merge_df[, -c(7:9)]
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
