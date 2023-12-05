# === Load data ==============================================
data_pull <- function(path = "03/data") {
  con <- file(path, "r")
  on.exit(close(con))

  out <- readLines(con)

  return(out)
}

raw_data <- data_pull()


# === Convert to a character matrix =====================
input_mat <- t(vapply(strsplit(raw_data, ""), \(x) x, character(140)))

# === Get symbols =======================================
symb <- unique(c(input_mat))[!grepl("[\\.0-9]", unique(c(input_mat)))]

# === Define adjacency filter ==========================
filter_mat <- rbind(
  c(1,     1,       1),
  c(1, NA_integer_, 1),
  c(1,     1,       1)
)

# Symbols will be coded as -1, numbers as-is, else NA
identify_numbers <- function(mat, filter = filter_mat) {
  # Pad matrix (assumes 3x3)
  pad_mat <- matrix(NA_integer_, nrow = nrow(mat) + 2, ncol = ncol(mat) + 2)
  pad_mat[1 + seq_len(nrow(mat)), 1 + seq_len(ncol(mat))] <- mat
  out <- matrix(NA_integer_, nrow = nrow(mat) + 2, ncol(mat) + 2)

  # Find hits
  idx <- which(pad_mat < 0, arr.ind = TRUE)

  for (i in seq_len(nrow(idx))) {
    point <- idx[i, ]

    x_seq <- seq(point["row"] - 1, point["row"] + 1)
    y_seq <- seq(point["col"] - 1, point["col"] + 1)

    out[x_seq, y_seq] <- pad_mat[x_seq, y_seq] * filter_mat
  }

  browser()
  # Return the position of numbers
  return(which(!is.na(out[1 + seq_len(nrow(mat)), 1 + seq_len(ncol(mat))]), arr.ind = TRUE))
}

# === Convert text into numbers so we can use functions downstream ======
char_to_int <- function(strs) {
  # HACK: This is hardcoded because I'm lazy
  out <- integer(length(str))

  for (i in seq_along(strs)) {
    out[i] <- switch(
        strs[i],
        "-" =,
        "@" =,
        "*" =,
        "=" =,
        "%" =,
        "/" =,
        "$" =,
        "#" =,
        "+" =,
        "&" = -1L,
        "." = NA_integer_,
        as.integer(strs[i])
      )
  }

  return(out)
}

# === Compute as integer matrix too (identifying symbols and numbers only) ========
input_int <- matrix(char_to_int(input_mat), ncol = ncol(input_mat))

# === Identify numbers next to symbols ==========================
indicated_numbers <- identify_numbers(input_int)

# === get ranges for numbers ====================================
# Return a lookup for positions of all numbers that we can filter
# by our results (which might only be the edge of a number)
number_positions <- gregexpr("[0-9]+", raw_data)

filter_number_positions <- function(arr_ind, positions, input_strs) {

  numbers <- integer(0)

  for (row in seq_along(positions)) {
    numbers_in_row <- positions[[row]]
    number_idx_to_filter <- arr_ind[arr_ind[, "row"] == row, ]

    nums_filtered <- integer(0)

    for (i in seq_along(numbers_in_row)) {
      num_idx <- seq(numbers_in_row[i], numbers_in_row[i] + attr(numbers_in_row, "match.length")[i] - 1)
      if (any(number_idx_to_filter[, "col"] %in% num_idx)) {
        nums_filtered <- c(nums_filtered, i)
      }
    }

    numbers <- c(numbers, as.integer(regmatches(input_strs[row], list(numbers_in_row))[[1]][nums_filtered]))
  }

  return(numbers)
}

filtered_numbers <- filter_number_positions(indicated_numbers, number_positions, raw_data)

sum(filtered_numbers)
