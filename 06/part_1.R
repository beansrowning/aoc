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

generate_race_wins <- function(time, dist_to_beat, v0 = 0, a = 1) {

  time_waiting <- 1L
  distances <- integer()

  while (time_waiting < time) {
    time_driving <- time - time_waiting
    v <- v0 + (a * time_waiting)

    distances <- c(distances, v * time_driving)
    time_waiting <- time_waiting + 1L
  }

  return(distances[distances > dist_to_beat])
}

total_ways <- Map(generate_race_wins, raw_data[[1]], raw_data[[2]]) |>
  vapply(length, integer(1)) |>
  (\(x) Reduce(`*`, x))()


