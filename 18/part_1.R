
# === Params =================================================
lookup <- c(
  "R" = "H",
  "L" = "H",
  "U" = "V",
  "D" = "V"
)


# === Load data ==============================================
data_pull <- function(path = "18/sample") {
  con <- file(path, "r")
  on.exit(close(con))

  raw_text <- strsplit(readLines(con), " ")

  out <- setNames(
    do.call(rbind.data.frame, args = raw_text),
    c("direction", "distance", "color")
  )
  out[["distance"]] <- as.integer(out[["distance"]])
  out[["color"]] <- gsub("[\\(|\\)]", "", out[["color"]]) 

  out[out[["direction"]] %in% c("L", "U"), "distance"] <-  -out[["distance"]][out[["direction"]] %in% c("L", "U")]
  out[["direction"]] <- lookup[out[["direction"]]]

  return(out)
}

# Pull instructions data frame
data <- data_pull()

# Determine x/y extents
extents <- split(data, ~direction) |>
  lapply(\(x) Reduce(`+`, x[["distance"]], accumulate = TRUE)) |>
  vapply(max, integer(1))

extents <- extents + 1L

# Build matrix
char_matrix <- matrix(NA_character_, nrow = extents[2], ncol = extents[1])

i <- j <- old_i <- old_j <- 1

for (row in split(data, seq_len(nrow(data)))) {
  browser()
  if (row[["direction"]] == "H") {
    old_j <- j
    j <- j + row[["distance"]]
    j_range <- do.call(seq, as.list(range(old_j, j)))

    char_matrix[i, j_range] <- row[["color"]]
  } else {
    old_i <- i
    i <- i + row[["distance"]]
    i_range <- do.call(seq, as.list(range(old_i, i)))

    char_matrix[i_range, j] <- row[["color"]]
  }
}

# Fill east-bound
for (idx in seq_len(nrow(char_matrix))) {

  vals <- na.omit(unique(char_matrix[idx, ]))
  n_unique <- length(vals)
  filled_pos <- cumsum(!is.na(char_matrix[idx, ]))

  fill_jdx <-  filled_pos > 0 & filled_pos != max(filled_pos) & is.na(char_matrix[idx, ])
  fill_patch <- vals[filled_pos[fill_jdx]]

  char_matrix[idx, fill_jdx] <- fill_patch
}

print(sum(!is.na(char_matrix)))