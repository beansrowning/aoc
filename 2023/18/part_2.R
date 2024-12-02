library(ggplot2)

# === Params =================================================
lookup1 <- c(
  "0" = "R",
  "1" = "D",
  "2" = "L",
  "3" = "U"
)

lookup2 <- c(
  "R" = 0i+1,
  "L" = 0i-1,
  "U" = 1i+0,
  "D" = -1i+0
)


# === Load data ==============================================
data_pull <- function(path = "18/data") {
  con <- file(path, "r")
  on.exit(close(con))

  raw_text <- scan(con, what = list(NULL, NULL, character(1)))[[3]]

  out <- gsub("\\)", "", gsub("\\(#", "0x", raw_text))
  out <- regmatches(out, gregexec("(^0x.*)(\\d$)", out))

  out <- vapply(out, \(x) lookup2[lookup1[x[3, ]]] * strtoi(x[2, ]), complex(1))

  return(out)
}

# Pull instructions data frame
vec <- data_pull()

# Each point is the result of the vector lining things up
# tip-to-tail
points <- Reduce(`+`, vec, accumulate = TRUE)

# Compute circumference
b <- Reduce(\(x, y) x + abs(Re(y)) + abs(Im(y)), vec, init = 0)


# Shoelace Theorem
area <- double(1)

for (i in seq_along(points)) {
  y_min <- ifelse(i - 1 == 0, points[length(points)], points[i - 1])
  y_max <- ifelse(i + 1 > length(points), points[1], points[i + 1])
  area <- area + (Re(points[i]) * (Im(y_max) - Im(y_min)))
}

area <- abs(area) * .5

# Pick's Theorem
interior <- area - (b / 2) + 1

total_area <- b + interior

options(scipen = 14)
print(total_area)
