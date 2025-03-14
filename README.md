# gofigR

gofigR is the R client for <https://gofigr.io>, a zero-effort reproducibility engine. It works with any R library which outputs to R graphics devices, with automatic publishing for `ggplot`.

## Compatibility

gofigR integrates with R markdown, both in `knitr` and in interactive sessions in RStudio. GoFigr also works in scripts. We tested with R 4.3.2 but any reasonably recent version should work.

GoFigr will automatically publish all `ggplot` output assuming you call `gofigR::enable(auto_publish=TRUE)`. GoFigr will *not* publish old-style R plots unless you tell it to. See the "Usage" section below.

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

```` rmd
```{r setup, include=FALSE}
library(gofigR)
gofigR::enable(auto_publish=TRUE)
```
````

`auto_publish` is FALSE by default. Set it to TRUE to override `plot` and `print` and publish figures automatically.

## Automatic output capture

If `auto_publish` is on, GoFigr will intercept all calls to `plot` and `print` and publish the results if they are from a compatible library. At the moment, we support any graphics format also supported by `ggplotify`:

-   ggplot2

-   ComplexHeatmap

-   cowplot

-   patchwork

-   lattice

## Manual capture

To capture output manually, simply call `publish`:

``` R
hm1 <- Heatmap(matrix(rnorm(100), nrow=10, ncol=10))

publish(hm1, "Heatmaps are cool!")  # second argument is the figure name
```

## Capturing base graphics

To capture output from base R plotting, call `publish_base`:

```         
gofigR::publish_base({
  base::plot(pressure, main="Pressure vs temperature")
  text(200, 50, "Note the non-linear relationship")
}, data=pressure, figure_name="Pressure vs temperature")

gofigR::publish_base({
  # The mtcars dataset:
  data <- as.matrix(mtcars)

  coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
  heatmap(data, scale="column", col = coul, main="Visualizing mtcars")
}, data=mtcars, figure_name="Cars")
```

Note the optional `data` argument following the expression. It specifies the data which you want to associate with the figure -- it will show up under "files" (as `.RDS`) once published.

## Interactive use

We support `knitr` as well as interactive sessions in `RStudio`.
