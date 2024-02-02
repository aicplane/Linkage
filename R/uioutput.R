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
    updateRadioGroupButtons(session, "Species", selected = "1")
    toggleState(id = "Species", condition = input$data == "3")
  }
  if (input$data == "2") {
    updateRadioGroupButtons(session, "Species", selected = "2")
    toggleState(id = "Species", condition = input$data == "3")
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
})

observeEvent(input$submit8, {
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
  if (input$data == "1" | input$data == "2") {
    output$datahelptext <- renderUI({
      tagList(
        div(
          p(
            "1. This shiny application can only be used to analyze bulk RNA-seq and bulk ATAC-seq;"
          ),
          #br(),
          "2. The data format must be consistent with the data format displayed below. The",
          code("first"),
          "column of RNA-seq must be the",
          strong("gene name"),
          "and the ",
          code("second to fourth"),
          " columns must be the ",
          strong("chromosome position;"),
          " The",
          code("first to third "),
          "columns of ATAC-seq must be the" ,
          strong("chromosome position."),
          align = "justify"
        )
        
      )
    })
  }
  
  if (input$data == "3") {
    output$datahelptext <- renderUI({
      NULL
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
        label = "Go Next",
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
               label = "Go Next",
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
        label = "Go Next",
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
               label = "Go Next",
               icon = icon("play")
             ),
             actionBttnParams
           )))
  }
})