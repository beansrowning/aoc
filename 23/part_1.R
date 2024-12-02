library(ggraph)
library(tidygraph)

# === Load data ==============================================
data_pull <- function(lookup, path = "23/sample") {
  con <- file(path, "r")
  on.exit(close(con))

  out <- do.call(rbind, strsplit(readLines(con), ""))

  return(out)
}

# Pull map
data <- data_pull()

# Get nodes
nodes <- which(data %in% c(".", "v", ">", "<", "^")) |>
  arrayInd(dim(data)) |>
  `colnames<-`(c("row", "col"))
