library(gofigR)
library(ggplot2)
library(plotly)
library(evaluate)

enable(analysis_name="My first Rmd analysis 2",
       watermark = gofigR::QR_WATERMARK,
       debug=TRUE)

plot(pressure, main="Script output")
