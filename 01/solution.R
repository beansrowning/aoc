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

numbers_lk <- setNames(as.character(1:9), numbers)

# === function ================================
get_number <- function(patterns, strs) {
  out <- integer(length(strs))

  inner_fx <- function(pattern, str) {
    out <- gregexpr(pattern, str)
    out[[1]][out[[1]] == -1] <- NA_integer_
    return(out[[1]])
  }

  for (i in seq_along(strs)) {
    # Determine first number
    reg_results <- lapply(patterns, inner_fx, strs[i])

    first_digit <- which.min(vapply(reg_results, min, integer(1)))
    last_digit <- which.max(vapply(reg_results, max, integer(1)))

    out[i] <- as.integer(paste(first_digit, last_digit, sep = ""))
  }

  return(out)
}

# === check =================
clean_data <- get_number(paste(names(numbers_lk), numbers_lk, sep = "|"), raw_data)

sum(clean_data)
# 54980 # Wrong

check_nums <- sample(seq_along(clean_data), 20)
setNames(clean_data[check_nums], raw_data[check_nums])