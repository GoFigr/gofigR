source('api.R')

gf <- gofigr.client(username=Sys.getenv("GF_USERNAME"),
                    password=Sys.getenv("GF_PASSWORD"),
                    verbose=TRUE)

print(list.workspaces(gf))
