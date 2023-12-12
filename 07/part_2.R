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
  "J" = 1,
  "Q" = 11,
  "K" = 12,
  "A" = 13
)

# Pull data, coded how we want
parsed_data <- data_pull(num_lk)

handle_joker <- function(hand) {
  joker_n <- sum(hand == 1)
  distinct_n <- length(unique(hand))
  # Early return if there are no jokers
  if (joker_n == 0) {
    return(hand)
  }

  joker_three_distinct_case <- function(hand) {
    # Case: two pair already
    # - Could be that a pair is already Js
    #   so we make it a four-of-a-kind
    # - Could be only 1 J, in which case a sub
    #   would make it a full house
    # In either scenario, the replacement should work the same
    # Case: three-of-a-kind already
    # - Could be 3x J, so we sub for max to make a four-of-a-kind
    # - Could be 1x J, so we sub for the value of three-of-a-kind to make four of a kind
    tab <- tabulate(hand)
    
    if (tab[1] == 3) {
      # Three jokers already, so sub those with the max value to make 4-of-a-kind
      return(max(hand))
    } else {
      # All other cases should work via this method
      return(max(which(tab == max(tab))))
    }
  }

  joker_four_distinct_case <- function(hand, n) {
    # Case: one-pair already
    # - 1J : sub for the value of the pair to make 3-of-a-kind
    # - 2J : sub for the highest value to make a 3-of-a-kind with that card
    if (n == 2) {
      return(max(hand))
    } else {
      return(which(tabulate(hand) == 2))
    }
  }

  replace_val <- switch(
    distinct_n,
    1, # No replacement, already 5-of-a-kind
    # Case: Four-of-a-kind
    # - 4x Jokers, in which case we just reassign to max to make a 5-of-a-kind
    # - 1x Jokers, in which case we can do the same because that will still make it a 5-of-a-kind
    # Case: Full house
    # - 3x Jokers or 2x jokers, in both cases we can just assign to the higher val
    #   and make it a 5-of-a-kind
    max(hand),
    # Need addn disambiguation for three and four distinct card case
    joker_three_distinct_case(hand),
    joker_four_distinct_case(hand, joker_n),
    # Case: High-card
    # - Just sub for highest card to make a pair
    max(hand)
  )

  hand[hand == 1] <- replace_val

  return(hand)
}

winnings <- function(hand) {
  # Part 2: handle jokers before determining hand strength
  new_hand <- handle_joker(hand)

  distinct_n <- length(unique(new_hand))
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
    four_distinct_case(new_hand), # disaggregate four-of-a-kind + 1 or full house
    three_distinct_case(new_hand), # disaggregate 3-of-a-kind from two-pair
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

total_winnings
# 249666369
