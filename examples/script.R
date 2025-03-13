library(gofigR)
library(ggplot2)
library(cowplot)
library(RColorBrewer)

knitr::opts_chunk$set(echo = TRUE)
enable()

gofigR::capture({
  plot(pressure, main="Pressure vs temperature")
  text(200, 50, "Note the non-linear relationship")
}, data=pressure, name="Pressure vs temperature")

gofigR::capture({
  # The mtcars dataset:
  data <- as.matrix(mtcars)

  coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
  heatmap(data, scale="column", col = coul, main="Visualizing mtcars")
}, name="Cars")

## Automatic figure capture for ggplot2

p <- ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, size=cyl)) +
  geom_point() +
  theme(legend.position="none") + ggtitle("Correlations")

print(p)

