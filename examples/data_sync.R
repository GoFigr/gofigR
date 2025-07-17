library(gofigR)

enable(analysis_name="My first Rmd analysis 2",
       watermark = gofigR::QR_WATERMARK,
       debug=TRUE)

gf <- get_client()

gofigR::sync_file("~/Downloads/expression.csv")
gofigR::sync_file("~/Downloads/Quick demo.ipynb")

plot(1:10, 1:10) %>% publish_base
