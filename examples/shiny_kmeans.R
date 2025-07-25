library(shiny)
library(gofigR)
gofigR::enable()

# Define UI for application that draws a histogram
ui <- function() {
  # k-means only works with numerical variables,
  # so don't give the user the option to select
  # a categorical variable
  vars <- setdiff(names(iris), "Species")

  pageWithSidebar(
    headerPanel('Iris k-means clustering'),
    sidebarPanel(
      selectInput('xcol', 'X Variable', vars),
      selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
      numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
    ),
    mainPanel(
      gfPlot('plot1')
    ))
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })

  gfPlotServer("plot1", {
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  }, input, figure_name = "kMeans clustering")

}

# Run the application
shinyApp(ui = ui, server = server)
