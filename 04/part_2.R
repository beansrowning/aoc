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

get_total_cards <- function(matches) {
  # This will grow after each iteration
  # based on our winnings
  n_won_cards <- 0L

  get_winnings <- function(idx) {
    return(seq(idx + 1, length.out = matches[idx]))
  }
  
  for (i in seq_along(matches)) {
    round_wins <- i
    n_round_wins <- 0L

    repeat {
      round_wins <- lapply(unlist(round_wins), get_winnings)
      n_round_wins <- n_round_wins + length(round_wins)

      if (length(unlist(round_wins)) == 0) {
        break
      }
    }

    n_won_cards <- n_won_cards + n_round_wins
  }


  # Length of the cards we won
  return(n_won_cards)
}

# How many scratch cards did we end up with?
total_cards <- get_total_cards(matches)

total_cards
# 5747443