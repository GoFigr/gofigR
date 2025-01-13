# gofigR

gofigR is the R client for https://gofigr.io, a zero-effort reproducibility
engine.

## Compatibility
gofigR integrates with R markdown, both in `knitr` and in interactive sessions in RStudio. We tested 
with R 4.3.2 but any reasonably recent version should work.

## Installation

```R
library(devtools)
devtools::install_github("https://github.com/gofigr/gofigR")
```

## Configuration

On the R prompt, simply load the `gofigR` package and call `gfconfig()`. 
You only need to do this once.

If you don't have an account, you can register at https://app.gofigr.io/register.

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

To enable GoFigr, simply call `enable` in your setup chunk. `analysis_name` specifies the analysis
under which all figures will be published (it will be created automatically
if it doesn't exist).

````Rmd
```{r setup, include=FALSE}
library(gofigR)

gofigR::enable(analysis_name="My first Rmd analysis")
```
````

You can then knit your markdown as-is. However, you can also customize GoFigr
through chunk options:

* `gofigr_figure_name`: manually specify the name of the figure
* `gofigr_on`: set to FALSE to disable GoFigr within a chunk

For example:

````Rmd
```{r pressure, echo=FALSE, gofigr_figure_name="My first figure"}
plot(pressure)
```
  
```{r pressure2, echo=FALSE, gofigr_on=FALSE}
# Won't be published
plot(pressure)
```
````

## Capturing output

GoFigr will attempt to intercept and publish all plots by default. You can also 
explicitly tell it to capture output from an expression:

```
gofigR::capture({
  plot(pressure, main="Pressure vs temperature")
  text(50, 50, "My pretty figure")
}, pressure)
```

This is handy when you build a plot iteratively. For example, you may call `plot(...)` first, followed
by a call to `text()` to add annotations, or `legend()` to place the legend. In that case you 
want to execute the entire block, as opposed to just `plot()`, before pushing output to GoFigr.

Note the argument following the expression (`pressure` in the snippet above). It specifies
the data which you want to associate with the figure -- it will show up under "files" (as `.RDS`) once published.

## Adding support for other plotting libraries

If you have a plotting function which GoFigr is not automatically detecting,
you can register it as follows:

```
barplot <- gofigR::intercept(graphics::barplot)
```

Subsequent calls to `barplot` will then automatically publish to GoFigr.

## Interactive use

gofigR works best with `knitr`, but interactive sessions within RStudio are also supported.

When running within RStudio, you will see both the original plots as well as their published & watermarked
counterparts.
