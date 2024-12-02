# === Load data ==============================================
data_pull <- function(lookup, path = "07/data") {
  con <- file(path, "r")
  on.exit(close(con))

  raw_text <- readLines(con)

  out <- strsplit(raw_text, " ") |>
      lapply(
        function(x){
        setNames(
          list(
            lookup[strsplit(x[1], "")[[1]]],
            x[2]
          ),
          c("hand", "bet")
        )
        }
      )



  return(out)
}

# Card lookup
num_lk <- c(
  setNames(2:9, as.character(2:9)),
  "T" = 10,
  "J" = 11,
  "Q" = 12,
  "K" = 13,
  "A" = 14
)

# Pull data, coded how we want
parsed_data <- data_pull(num_lk)

winnings <- function(hand) {
  distinct_n <- length(unique(hand))

  # Scores:
  # five-of-a-kind: 7
  # four-of-a-kind: 6
  # full house: 5
  # three-of-a-kind: 4
  # two-pair: 3
  # one-pair: 2
  # high card: 1

  # Four-of-a-kind if 1 (individual card) or 4 (one of the 4)
  # Full house if 2 (one of the pair) or 3 (one of the 3-of-a-kind)
  four_distinct_case <- \(x) switch(length(x[x == x[1]]), 6, 5, 5, 6)

  # Three-of-a-kind if one card is repeated 3x
  # two pair otherwise
  three_distinct_case <- \(x) c(3, 4)[any(tabulate(x) == 3) + 1]

  switch(
    distinct_n,
    7, # 1 distinct number has to be 5-of-a-kind
    four_distinct_case(hand), # disaggregate four-of-a-kind + 1 or full house
    three_distinct_case(hand), # disaggregate 3-of-a-kind from two-pair
    2, # Must be exactly 1 pair (4 distinct cards, 1 must repeat)
    # high card (encoding the highest card as a fractional component out of 1 + n)
    1
  )
}


winnings_mat <- cbind(
  idx = seq_along(parsed_data),
  base_win = vapply(parsed_data, \(x) winnings(x[["hand"]]), double(1)),
  bet = vapply(parsed_data, \(x) as.numeric(x[["bet"]]), double(1))
)

card_mat <- t(vapply(parsed_data, \(x) x[["hand"]], double(5)))
colnames(card_mat) <- sprintf("card_%d", 1:5)

full_mat <- cbind(winnings_mat, card_mat)

full_mat_sort <- full_mat[
  order(
    full_mat[, "base_win"],
    full_mat[, "card_1"],
    full_mat[, "card_2"],
    full_mat[, "card_3"],
    full_mat[, "card_4"],
    full_mat[, "card_5"],
    decreasing = TRUE
  ),
]

total_winnings <- sum(full_mat_sort[, "bet"] * rev(seq_len(nrow(full_mat_sort))))
# 249204891