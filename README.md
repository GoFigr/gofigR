# gofigR

gofigR is the R client for https://gofigr.io, a zero-effort reproducibility
engine.

## Compatibility
gofigR integrates with R markdown (Rmd), both within `knitr` and interactive sessions in RStudio. We tested 
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

## Publishing other plot types


## Interactive use

gofigR currently only works when knit with `knitr`. Interactive sessions
within RStudio are not currently supported (but coming soon!).
