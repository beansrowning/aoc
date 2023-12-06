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
get_number <- function(strs) {
  out <- integer(length(strs))

  reg_results <- gregexec("\\d{1}", strs)
  for (i in seq_along(strs)) {

    first_digit <- substr(strs[i], min(reg_results[[i]]), min(reg_results[[i]]))
    last_digit <- substr(strs[i], max(reg_results[[i]]), max(reg_results[[i]]))

    out[i] <- as.integer(paste(first_digit, last_digit, sep = ""))
  }

  return(out)
}

# === check =================
clean_data <- get_number(raw_data)

sum(clean_data)
# 55816 # Correct