# === Load data ==============================================
data_pull <- function(path = "01/data") {
  con <- file(path, "r")
  on.exit(close(con))

  out <- readLines(con)

  return(out)
}

raw_data <- data_pull()

# === function ================================
get_number <- function(strs) {
  reg_results <- regmatches(strs, gregexec("\\d{1}", strs))
  out <- vapply(reg_results, \(x) (10 * as.double(x[1])) + as.double(x[length(x)]), double(1))

  return(out)
}

# === check =================
clean_data <- get_number(raw_data)

sum(clean_data)
# 55816