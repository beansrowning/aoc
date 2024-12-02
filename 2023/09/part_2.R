# === Load data ==============================================
data_pull <- function(lookup, path = "09/data") {
  con <- file(path, "r")
  on.exit(close(con))

  raw_text <- readLines(con)

  out <- t(vapply(strsplit(raw_text, " "), as.numeric, double(21)))

  return(out)
}

data <- data_pull()

get_prev_seq <- function(seq) {
  # This extract degree of polynomial too
  i <- 0L
  terms_to_add <- seq[1]

  repeat {
    seq <- diff(seq)

    if (all(seq == 0)) {
      break
    }
    terms_to_add <- c(terms_to_add, seq[1])
    i <- i + 1L
  }

  result <- Reduce(\(x, y) y - x, rev(terms_to_add), accumulate = FALSE)

  return(result)
}

prev_seq <- apply(data, 1, get_prev_seq)
totals <- sum(prev_seq)
# 1087