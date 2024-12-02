# === Load data ==============================================
data_pull <- function(lookup, path = "09/data") {
  con <- file(path, "r")
  on.exit(close(con))

  raw_text <- readLines(con)

  out <- t(vapply(strsplit(raw_text, " "), as.numeric, double(21)))

  return(out)
}

data <- data_pull()

get_next_seq <- function(seq) {
  # This extract degree of polynomial too
  i <- 0L
  terms_to_add <- seq[length(seq)]

  repeat {
    seq <- diff(seq)

    if (all(seq == 0)) {
      break
    }
    terms_to_add <- c(terms_to_add, seq[length(seq)])
    i <- i + 1L
  }

  result <- Reduce(`+`, rev(terms_to_add), accumulate = FALSE)

  return(result)
}

next_seq <- apply(data, 1, get_next_seq)
totals <- sum(next_seq)
# 1731106378