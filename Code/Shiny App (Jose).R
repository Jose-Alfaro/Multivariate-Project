dfHclust = function(df) {
  # validate input
  stopifnot(inherits(df, "data.frame"))
  stopifnot(ncol(df) > 1)
  # obtain software
  require(shiny)
  require(cluster)
  require(factoextra)
  # global variables ...
  nms = names(df)
  cmeths = c(
    "ward.D",
    "ward.D2",
    "single",
    "complete",
    "average",
    "mcquitty",
    "median",
    "centroid"
  )
  dmeths = c("euclidean", "maximum", "manhattan", "canberra",
             "binary")
  
  ui <- fluidPage(
    #
    # we will have four components on sidebar: selectors for
    # distance, agglomeration method, height for tree cut, and variables to use
    #
    titlePanel("Clustering Application"),
    tags$em("By: Jose Alfaro"),
    tags$hr(),
    
    sidebarPanel(
      helpText(paste("Select Distance:")),
      fluidRow(
        selectInput("dmeth", NULL, choices = dmeths,
                    selected = dmeths[1])
      ),
      helpText(paste("Select Clustering Method:")),
      fluidRow(selectInput(
        "meth", NULL, choices = cmeths,
        selected = cmeths[1]
      )),
      helpText(paste("Select Height for Cut:")),
      fluidRow(
        numericInput(
          "cutval",
          NULL,
          value = 40,
          min = 0,
          max = Inf,
          step = 1
        )
      ),
      helpText(paste("Select Number of Groups:")),
      fluidRow(
        numericInput(
          "clustgroup",
          NULL,
          value = 3,
          min = 0,
          max = Inf,
          step = 1
        )
      ),
      helpText(paste("Select Number of Clusters:")),
      fluidRow(
        numericInput(
          "clustval",
          NULL,
          value = 3,
          min = 0,
          max = Inf,
          step = 1
        )
      ),
      helpText(
        paste("Select Variables for Clustering From", substitute(df), ":")
      ),
      fluidRow(
        checkboxGroupInput("vars", NULL, choices = nms,
                           selected = nms[4:6])
      )
    ),
  
    mainPanel(tabsetPanel(
      tabPanel("Pairs",
               plotOutput("pairsplot")),
      tabPanel("Tree",
               plotOutput("plot1"),
               htmlOutput("txt", align = "left"),
               htmlOutput("txt2", align = "left")),
      tabPanel("K-Means",
               plotOutput("kplot")),
      tabPanel("Silh",
               plotOutput("silplot"))
    ))
  )  # end fluidPage
  
  
  ### Server Function ###
  server <- function(input, output) {
    output$plot1 <- renderPlot({
      
      output$pairsplot <- renderPlot({
        xv = df[, input$vars]
        pairs(scale(xv), pch = 16)
      })
      xv = df[, input$vars]
      # plot(
      #   hclust(dist(data.matrix(xv), method = input$dmeth), method = input$meth),
      #   xlab = paste(input$dmeth, "distance;", input$meth, "clustering"), labels = df$Brand
      # )
      # abline(h = input$cutval,
      #        lty = 2,
      #        col = "gray")
      # hc <- xv %>% dist(method = input$dmeth) %>% hclust(method = input$cmeth)
      hc <- hclust(dist(scale(xv), method = input$dmeth), method = input$meth)
      fviz_dend(hc, k = input$clustgroup, cex = 0.5, k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
                color_labels_by_k = T, rect = T, show_labels = T)      
    })
    
    output$kplot <- renderPlot({
      xv <- scale(df[, input$vars])
      set.seed(123)
      km <- kmeans(xv, centers = input$clustval, nstart = 50)
      fviz_cluster(
        km,
        data = xv,
        ellipse.type = "convex",
        palette = "jco",
        ggtheme = theme_minimal()
      )
      # for (i in 2:4)
      # {
      #   km = kmeans(xv, centers = input$clustval, nstart = 50)
      #   print(km$tot.withinss)
      # }
      #
    })
    
    output$silplot <- renderPlot({
      xv = df[, input$vars]
      dm = dist(data.matrix(xv), method = input$dmeth)
      hc = hclust(dist(data.matrix(xv), method = input$dmeth), method =
                    input$meth)
      ct = cutree(hc, h = input$cutval)
      plot(silhouette(ct, dm))
    })
    
    #Plots Text in Pairs Plots
    output$txt <- renderText({
      HTML(
        "Distance Method: "
      )
    })
    
    #Plots Text in Pairs Plots
    output$txt2 <- renderText({
      HTML(
        "Clustering Method: "
      )
    })
    
  }
  
  shinyApp(ui, server)
}
dfHclust(dta)
