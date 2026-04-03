library(plumber)
pr <- plumb("api.R")
pr$run(host = "0.0.0.0", port = as.integer(Sys.getenv("PORT", 8000)))