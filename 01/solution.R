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
get_number <- function(patterns, strs, which = c("min", "max")) {
  op_1 <- switch(
    match.arg(which),
    min = min,
    max = max,
  )

  op_2 <- switch(
    match.arg(which),
    min = which.min,
    max = which.max,
  )

  inner_fn <- function(pattern, strs) {
    out <- gregexec(pattern, strs)
    out <- vapply(out, op_1, integer(1))
    return(out)
  }

  reg_result <- vapply(patterns, inner_fn, integer(length(strs)), strs)

  reg_result[reg_result == -1] <- NA_integer_

  return(apply(reg_result, 1, op_2))
}

assemble_number <- function(lk, input) {
  first_num <- get_number(paste(names(lk), lk, sep = "|"), input, "min")
  last_num <- get_number(paste(names(lk), lk, sep = "|"), input, "max")

  out <- paste(first_num, last_num, sep = "")

  return(as.numeric(out))
}

# === check =================
clean_data <- assemble_number(numbers_lk, raw_data)

sum(clean_data)
# 54980 # Wrong

check_nums <- sample(seq_along(clean_data), 20)
setNames(clean_data[check_nums], raw_data[check_nums])