library(ggplot2)

# === Params =================================================
lookup <- c(
  "R" = 0i+1,
  "L" = 0i-1,
  "U" = 1i+0,
  "D" = -1i+0
)


# === Load data ==============================================
data_pull <- function(path = "18/data") {
  con <- file(path, "r")
  on.exit(close(con))

  out <- strsplit(readLines(con), " ")
  return(out)
}

# Pull instructions data frame
data <- data_pull()

# To complex
vec <- vapply(data, \(x) as.integer(x[2]) * lookup[x[1]], complex(1))
vec <- setNames(vec, vapply(data, \(x) gsub("[\\(|\\)]", "", x[3]), character(1)))

# Each point is the result of the vector lining things up
# tip-to-tail
points <- Reduce(`+`, vec, accumulate = TRUE)

# Compute circumference
b <- vapply(data, \(x) as.integer(x[2]), integer(1)) |>
  sum()


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
print(total_area)

# === Plot our task =======================================
ggplot(data.frame(NULL), aes(x = Re(points), y = Im(points))) +
  geom_path(aes(color = names(vec), group = 1), show.legend = FALSE) +
  scale_color_identity() +
  theme_bw()
