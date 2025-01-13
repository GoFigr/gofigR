library(gofigR)
library(ggplot2)
library(plotly)

enable(analysis_name="My first Rmd analysis 2",
       watermark = gofigR::QR_WATERMARK,
       verbose=FALSE)
plot(pressure, main="Pressure vs temperature")

gp <- ggplot(data=pressure, mapping=aes(x=temperature, y=pressure))
gp <- gp + geom_line()
print(gp)
