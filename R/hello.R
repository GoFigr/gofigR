library(httr)
library(jsonlite)

API_KEY <- ""

res <- httr::GET("https://api.gofigr.io/api/v1.2/user/",
                 add_headers(Authorization = paste0('Token ', API_KEY)))
data <- fromJSON(rawToChar(res$content))
print(data)
