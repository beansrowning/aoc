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
identify_gear_edges <- function(mat, filter = filter_mat) {
  # Pad matrix (assumes 3x3)
  pad_mat <- matrix(NA_integer_, nrow = nrow(mat) + 2, ncol = ncol(mat) + 2)
  pad_mat[1 + seq_len(nrow(mat)), 1 + seq_len(ncol(mat))] <- mat

  # Our list will be the coordinates of the edges of two numbers that
  # need to be identified and then multiplied together
  out <- list()

  # Find hits (symbols of interest should be coded -1)
  idx <- which(pad_mat < 0, arr.ind = TRUE)

  for (i in seq_len(nrow(idx))) {
    point <- idx[i, ]

    x_seq <- seq(point["row"] - 1, point["row"] + 1)
    y_seq <- seq(point["col"] - 1, point["col"] + 1)

    ratios <- which(!is.na(pad_mat[x_seq, y_seq] * filter_mat), arr.ind = TRUE)

    # This must be at least 2 (edge of two numbers)
    # but could be > 2 (multiple digits around symbol from 1 or both numbers)
    # We'll handle the >2 case down the line, so no worries.
    if (nrow(ratios) >= 2) {
      # NOTE: Not subtracting one to re-adjust for padding
      ratios[, 1] <- (ratios[, 1] - 2) + point["row"] - 1
      ratios[, 2] <- (ratios[, 2] - 2) + point["col"] - 1


      out <- c(out, list(ratios))
    }
  }


  # Return the position of numbers to multiply
  return(out)
}

# === Convert text into numbers so we can use functions downstream ======
char_to_int <- function(strs) {
  # HACK: This is hardcoded because I'm lazy
  out <- integer(length(str))

  for (i in seq_along(strs)) {
    out[i] <- switch(
        strs[i],
        "*" = -1L,
        "." = NA_integer_,
        as.integer(strs[i])
      )
  }

  return(out)
}

# === Compute as integer matrix too (identifying symbols and numbers only) ========
input_int <- matrix(char_to_int(input_mat), ncol = ncol(input_mat))

# === Identify numbers next to symbols ==========================
indicated_numbers <- identify_gear_edges(input_int)

# === get ranges for numbers ====================================
# Return a lookup for positions of all numbers that we can filter
# by our results (which might only be the edge of a number)
number_positions <- gregexpr("[0-9]+", raw_data)
all_numbers <- regmatches(raw_data, number_positions)

abind <- function(x, y) {
  dim_x <- dim(x)
  dim_y <- dim(y)

  stopifnot(identical(dim_x[1:2], dim_y[1:2]))

  return(array(c(x, y), dim = c(dim_x[1], dim_x[2], dim_x[3] + dim_y[3]), dimnames = dimnames(y)))
}

num_pos_to_lk <- function(row_num, reg_match, data) {
  out <- array(dim = c(3, 3, length(data)), dimnames = list(NULL, c("row", "col", "num"), NULL))

  for (i in seq_along(data)) {
    
    inner_out <- cbind(
      row = row_num,
      col = seq(c(reg_match)[i], c(reg_match)[i] + attr(reg_match, "match.length")[i] - 1),
      num = rep(as.integer(data)[i], times = attr(reg_match, "match.length")[i])
    )

    pad_n <- 3 - nrow(inner_out)

    if (pad_n) {
      inner_out <- rbind(
        inner_out,
        cbind(
          row = rep(NA_integer_, times = pad_n),
          col = rep(NA_integer_, times = pad_n),
          num = rep(NA_integer_, times = pad_n)
        )
      )
    }

    out[, , i] <- inner_out
  }


  return(out)
}

all_num_lk <- array(dim = c(3, 3, 1), dimnames = list(NULL, c("row", "col", "num"), NULL))

for (i in seq_along(number_positions)) {
  all_num_lk <- abind(
    all_num_lk,
    num_pos_to_lk(i, number_positions[[i]], all_numbers[[i]])
  )
}

# Remove our NA row at the top
all_num_lk <- all_num_lk[-1, ]

# Return full numbers from number edges and multiply together
gear_edges_to_gear_ratio <- function(arr_ind, positions) {

  numbers <- integer(0)

  for (pair_idx in seq_along(arr_ind)) {
    possible_edges <- arr_ind[[pair_idx]]
    num_idx <- apply(possible_edges, 1, \(x) which(positions[, "row", ] == x["row"] & positions[, "col", ] == x["col"], arr.ind = TRUE))
    # We could match multiple positions in the same number, so
    # only take the number once
    gears <- unique(num_idx[2, ])

    if (length(gears) == 2) {
      gear_ratio <- na.omit(unique(positions[, "num", gears[1]])) * na.omit(unique(positions[, "num", gears[2]]))

      numbers <- c(numbers, gear_ratio)
    }
  }
  return(numbers)
}

gear_ratios <- gear_edges_to_gear_ratio(indicated_numbers, all_num_lk)

sum(gear_ratios)
