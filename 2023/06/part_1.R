# === Load data ==============================================
data_pull <- function(path = "06/data") {
  con <- file(path, "r")
  on.exit(close(con))

  raw_text <- readLines(con)

  list_elem <- regmatches(raw_text, regexec("(.*):(.*)", raw_text))

  out <- list_elem |>
    lapply(\(x) setNames(list(as.integer(strsplit(x[3], "\\s+")[[1]][-1])), x[2])) |>
    unlist(recursive = FALSE)

  return(out)
}

raw_data <- data_pull()


v0 <- 0
a <- 1 # (per sec held down, must be int)

# The formula of the race can be represented as a quadratic, where the roots
# define the minimum and maximum time one can wait and still beat the time
find_roots <- function(time, dist, v0 = 0, accel = 1) {
  quadratic <- list(a = -1 * accel, b = (accel * time) + v0, c = -dist)

  roots <- function(a, b, c) {
    c(
      # If this is fractional, we have to account
      # for the challenge which is integer based
      ceiling(
        (-b + sqrt(b ^ 2 - 4 * a * c)) / 
        (2 * a)
      ),
      floor(
        (-b - sqrt(b ^ 2 - 4 * a * c)) / 
        (2 * a)
      )

    )
  }

  return(do.call(roots, quadratic))
}

seq_to_len <- function(start, stop) {
  stop - start + 1
}

total_ways <- Map(find_roots, raw_data[[1]], raw_data[[2]]) |>
  vapply(\(x) x[2] - x[1] + 1, numeric(1)) |>
  (\(x) Reduce(`*`, x))()

