# gofigR

gofigR is the R client for <https://gofigr.io>, a zero-effort reproducibility engine. It works with any R library which outputs to R graphics devices.

## Compatibility

gofigR integrates with R markdown, both in `knitr` and in interactive sessions in RStudio. GoFigr also works in scripts. We tested with R 4.3.2 but any reasonably recent version should work.

## Installation

``` r
library(devtools)
devtools::install_github("gofigr/gofigR")
```

## Configuration

On the R prompt, simply load the `gofigR` package and call `gfconfig()`. You only need to do this once.

If you don't have an account, you can register at <https://app.gofigr.io/register>.

```         
> library(gofigR)
Attaching package: ‘gofigR’

> gfconfig()
-------------------------------------------------------------------

Welcome to GoFigr! This wizard will help you get up and running.

-------------------------------------------------------------------


Username: publicdemo
Testing connection...

  => Success

API key (leave blank to generate a new one): 
Key name (e.g. Alyssa's laptop): my API key
Fetching workspaces...

1. Scratchpad - e5249bed-40f0-4336-9bd3-fef30d3ed10d

Please select a default workspace (1-1): 1

Configuration saved to /Users/maciej/.gofigr. Happy analysis!
```

## Usage

To enable GoFigr, simply call `enable` in your setup chunk. You can also optionally specify an `analysis_name` (it will be created automatically if it doesn't exist).

```` {.R .rmd}
```{r setup, include=FALSE}
library(gofigR)
gofigR::enable()
```
````

## Publishing plots

To publish plots, simply call `publish`:

``` r
hm1 <- Heatmap(matrix(rnorm(100), nrow=10, ncol=10))

publish(hm1, "Heatmaps are cool!")  # second argument is the figure name

# This works, too
hm1 %>% publish("Heatmaps are cool!")
```

## Capturing base graphics

To capture output from base R plotting, call `publish` with a plotting expression:

``` r
gofigR::publish({
  base::plot(pressure, main="Pressure vs temperature")
  text(200, 50, "Note the non-linear relationship")
}, data=pressure, figure_name="Pressure vs temperature")

gofigR::publish({
  # The mtcars dataset:
  data <- as.matrix(mtcars)

  coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
  heatmap(data, scale="column", col = coul, main="Visualizing mtcars")
}, data=mtcars, figure_name="Cars")
```

Note the optional `data` argument following the expression. It specifies the data which you want to associate with the figure -- it will show up under "files" (as `.RDS`) once published.

## Shiny

You can replace `plotOutput + renderPlot` with `gfPlot + gfPlotServer` and give your users the ability to publish interactive plots to GoFigr. For example:

``` r
library(shiny)
library(gofigR)

gofigR::enable()

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      gfPlot("distPlot"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  gfPlotServer("distPlot", {
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  }, input, figure_name="Old faithful waiting times")
}

# Run the application
shinyApp(ui = ui, server = server)
```

Note that we pass `input` to `gfPlotServer`. This will capture your current Shiny inputs – they will become available under the "Metadata" tab in GoFigr.

## Interactive use

We support `knitr` as well as interactive sessions in `RStudio`.
