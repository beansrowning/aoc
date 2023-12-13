# === Load data ==============================================
data_pull <- function(path = "01/data") {
  con <- file(path, "r")
  on.exit(close(con))

  out <- readLines(con)

  return(out)
}

raw_data <- data_pull()

# === Lookup table ===========================================
numbers <- c(
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine"
)

reg <- sprintf("(?=(%s|\\d))", paste(numbers, collapse = "|"))

numbers_lk <- setNames(as.character(1:9), numbers)
numbers_lk <- c(numbers_lk, setNames(1:9, as.character(1:9)))

# === function ================================
get_number <- function(pattern, strs) {
  out <- integer(length(strs))

  all_matches <- regmatches(strs, gregexec(pattern, strs, perl = TRUE))

  for (i in seq_along(all_matches)) {
    # extract numbers
    nums <- all_matches[[i]][2, c(1, ncol(all_matches[[i]]))]
    nums <- as.integer(numbers_lk[nums])

    out[i] <- (10 * nums[1]) + nums[2]
  }

  return(out)
}

# === check =================
clean_data <- get_number(reg, raw_data)

sum(clean_data)
# 54980
