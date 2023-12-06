# === Load data ==============================================
data_pull <- function(path = "02/data") {
  con <- file(path, "r")
  on.exit(close(con))

  out <- readLines(con)

  return(out)
}

raw_data <- data_pull()

# === Game Parameters =======================================
bag_contents <- cbind(
  red = 12,
  green = 13,
  blue = 14
)
# === Solver =========================================
get_count <- function(strs, color = c("red", "green", "blue")) {
  color <- match.arg(color, several.ok = FALSE)

  out <- integer(length(strs))
  seq_match <- regmatches(strs, gregexec(sprintf("(\\d*)( %s,*)", color), strs))

  for (i in seq_along(seq_match)) {
    match_val <- seq_match[[i]]

    if (!length(match_val)) {
      n <- 0L
    } else {
      n <- as.integer(match_val[2, 1])
    }

    out[i] <- n
  }

  return(out)
}

check_games <- function(output, params) {
  ids <- sub("(^Game (\\d{1,3}): )(.*)$", "\\2", output)
  game_results <- sub("(^Game (\\d{1,3}): )(.*)$", "\\3", output)

  rounds <- strsplit(game_results, ";")

  pass <- logical(length(output))

  for (i in seq_along(rounds)) {
      rounds_mat <- cbind(
        get_count(rounds[[i]], "red"),
        get_count(rounds[[i]], "green"),
        get_count(rounds[[i]], "blue")
      )

    pass[i] <- all(apply(rounds_mat, 1, \(x) !any(params - x < 0)))
  }

  return(setNames(pass, ids))
}

# === Solve =====================================
# Check games
ans <- check_games(raw_data, bag_contents)

# Determine sum
sum(as.numeric(names(ans[ans])))
# 2913