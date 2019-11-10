setwd("C:/Users/josea/Desktop/Stat 7331 Multivariate")
df <- read.csv("cereal.csv")
# df <- multishapes[, 1:2]
# df <- df[sample(nrow(df), 750), ]
# validate input
stopifnot(inherits(df, "data.frame"))
stopifnot(ncol(df) > 1)
# obtain software
require(shiny)
require(cluster)
require(factoextra)
library(fpc)
library(dbscan)
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
    helpText(paste("Select Linkage Method:")),
    fluidRow(selectInput(
      "meth", NULL, choices = cmeths,
      selected = cmeths[1]
    )),
    # helpText(paste("Select Height for Cut:")),
    # fluidRow(
    #   numericInput(
    #     "cutval",
    #     NULL,
    #     value = 40,
    #     min = 0,
    #     max = Inf,
    #     step = 1
    #   )
    # ),
    helpText(paste("Select Number of Groups (H-Clust):")),
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
    helpText(paste("Select Number of Clusters (K-Means):")),
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
    helpText(paste("Select Minimum Number of Points (DBSCAN):")),
    fluidRow(
      numericInput(
        "dbval",
        NULL,
        value = 5,
        min = 0,
        max = Inf,
        step = 1
      )
    ),
    helpText(paste("Select Epsilon Value (DBSCAN):")),
    fluidRow(
      numericInput(
        "dbval2",
        NULL,
        value = 0.15,
        min = 0,
        max = Inf,
        step = .05
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
             plotOutput("kplot"),
             plotOutput("kelbow"),
             plotOutput("sil")),
    tabPanel("DBSCAN",
             plotOutput("dbscan"))
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
    rownames(xv) <- xv$Brand
    fviz_dend(hc, k = input$clustgroup, cex = 0.5, k_colors = c("#2E9FDF", "#B19CD9", "#5BC8AC", "#E6D72A", "#F18D9E"),
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
  })
  
  output$kelbow <- renderPlot({
    xv <- scale(df[, input$vars])
    set.seed(123)
    # function to compute total within-cluster sum of square 
    wss <- function(k) {
      kmeans(xv, k, nstart = 50 )$tot.withinss
    }
    
    # Compute and plot wss for k = 1 to k = 10
    k.values <- 1:6
    
    # extract wss for 2-15 clusters
    wss_values <- map_dbl(k.values, wss)
    
    plot(k.values, wss_values,
         type="b", pch = 19, frame = FALSE, 
         xlab="Number of Clusters (K)",
         ylab="Total Within-Clusters Sum of Squares")
    
  })
  
  output$sil <- renderPlot({
    xv <- scale(df[, input$vars])
    set.seed(123)
    # function to compute average silhouette for k clusters
    avg_sil <- function(k) {
      km.res <- kmeans(xv, centers = k, nstart = 50)
      ss <- silhouette(km.res$cluster, dist(xv))
      mean(ss[, 3])
    }
    
    # Compute and plot wss for k = 2 to k = 15
    k.values <- 2:6
    
    # extract avg silhouette for 2-15 clusters
    avg_sil_values <- map_dbl(k.values, avg_sil)
    
    plot(k.values, avg_sil_values,
         type = "b", pch = 19, frame = FALSE, 
         xlab = "Number of Clusters (K)",
         ylab = "Average Silhouettes")
  })
  
  output$dbscan <- renderPlot({
    xv = df[, input$vars]
    set.seed(123)
    db <- dbscan(xv, eps = input$dbval2, MinPts = input$dbval)
    fviz_cluster(db, xv, stand = FALSE, frame = FALSE, geom = "point")
  })
  
  #Plots Text in Pairs Plots
  output$txt <- renderText({
    HTML(
      ""
    )
  })
  
  #Plots Text in Pairs Plots
  output$txt2 <- renderText({
    HTML(
      ""
    )
  })
  
}

shinyApp(ui, server)