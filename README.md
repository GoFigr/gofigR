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

Username: demo
Password: *******************
Testing connection...
  => Success
API key (leave blank to generate a new one): 
Key name (e.g. Alyssa's laptop): work laptop
Fetching workspaces...


| Number|Name              |Description                  |API.ID                               |
|------:|:-----------------|:----------------------------|:------------------------------------|
|      1|Primary Workspace |demouser's primary workspace |98f328fc-f984-482c-b7f6-8ed272527a42 |
|      2|Rocketship Bio    |Let's do some science        |bff6c952-fb2c-4333-80e5-5dc7291d47cc |
|      3|Plotly demos      |N/A                          |1424ea0f-7d42-4ef5-9ba8-41aa9c5b1a94 |

Please select a default workspace (1-3): 1

Configuration saved to /Users/maciej/.gofigr. Happy analysis!

> 
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

If `auto_publish` is on, GoFigr will intercept all calls to `plot` and `print` and publish the results if they are from a compatible library (at the moment, only `ggplot`).

## Manual capture

To capture output from base R plotting, including when the plot is built iteratively across multiple expressions, wrap your code in `gofigR::capture`:

```         
gofigR::capture({
  plot(pressure, main="Pressure vs temperature")
  text(50, 50, "My pretty figure")
}, data=pressure, name="Pressure plot")
```

This is handy when you build a plot iteratively. For example, you may call `plot(...)` first, followed by a call to `text()` to add annotations, or `legend()` to place the legend.

Note the optional `data` argument following the expression. It specifies the data which you want to associate with the figure -- it will show up under "files" (as `.RDS`) once published.

## Adding support for other plotting libraries

If you have a plotting function which you use often and which you would like to auto-publish, you can use `intercept`:

```         
barplot <- gofigR::intercept(graphics::barplot)
```

Subsequent calls to `barplot` will then automatically publish to GoFigr.

## Interactive use

gofigR works best with `knitr`, but interactive sessions within RStudio are also supported.
