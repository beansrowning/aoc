# === Load data ==============================================
data_pull <- function(path = "04/data") {
  con <- file(path, "r")
  on.exit(close(con))

  out <- readLines(con)

  return(out)
}

raw_data <- data_pull()

# Assemble our two lists for each card
split_data <- strsplit(sub("Card\\s* \\d*: ", "", raw_data), " \\| ")
split_clean <- lapply(split_data, \(x) regmatches(x, gregexpr("[0-9]+", x)))

# Determine how many matches per card we have
matches <- vapply(split_clean, \(x) sum(x[[2]] %in% x[[1]]), integer(1))

# Convert this to a score (this is dumb, but I'm lazy)
score <- ifelse(matches == 0, 0, 2 ^ (matches - 1))

sum(score)
#22674