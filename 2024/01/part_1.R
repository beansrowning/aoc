data_pull <- function(path = "2024/01/data") {
  con <- file(path, "r")
  on.exit(close(con))

  out <- read.table(con)

  return(out)
}

# Read in data
data <- data_pull()

ans <- sum(abs(mapply(`-`, sort(data[[1]]), sort(data[[2]]))))

print(ans)