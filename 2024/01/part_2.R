data_pull <- function(path = "2024/01/data") {
  con <- file(path, "r")
  on.exit(close(con))

  out <- read.table(con)

  return(out)
}

data <- data_pull()

# === Determine which nums are in x ======
matches <- intersect(data[[1]], data[[2]])

# Get a tabulation
counts_left <- tabulate(data[[1]][data[[1]] %in% matches])
counts_right <- tabulate(data[[2]][data[[2]] %in% matches])

counts <- counts_left * counts_right

# multiply and sum
ans <- sum(counts * seq_along(counts))

print(ans)
