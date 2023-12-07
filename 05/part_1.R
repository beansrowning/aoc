# === Load data ==============================================
data_pull <- function(path = "05/data") {
  con <- file(path, "r")
  on.exit(close(con))

  out <- readLines(con)

  return(out)
}

raw_data <- data_pull()

# === Functions ==========================================================
build_maps <- function(raw_text) {

  map_names <- regmatches(raw_text, regexec("(.*)( map:)", raw_text))
  map_names <- vapply(map_names[vapply(map_names, length, integer(1)) > 0], \(x) x[2], character(1))

  out <- vector("list", length = length(map_names))
  out <- setNames(out, map_names)

  # Adding final line for last map
  map_extents <- which(raw_text == "")
  map_extents <- c(map_extents, length(raw_text) + 1)

  # Iterator will be -1 map starts so the last map extent
  # i - i + 1 will work
  for (i in seq_along(out)) {
    # + 2 skips header row
    map_spans <- seq(map_extents[i] + 2, map_extents[i + 1] - 1)
    span_list <- regmatches(raw_text[map_spans], gregexpr("[0-9]+", raw_text[map_spans]))
    # Sneaky sneaky, integer type will overflow.
    # could use int64, but I'm going base-R so I'll use numeric
    out[[i]] <- t(vapply(span_list, as.numeric, numeric(3)))
  }

  return(out)
}

# find a destination for input source
# present in map
eval_source_in_map <- function(input, map) {

  # sort map to maximize our speed here
  map <- map[order(map[, 2]), ]

  i <- 1L
  dest <- numeric(0)

  while (i <= nrow(map)) {
    if (input >= map[i, 2] && input <= map[i, 2] + map[i, 3]) {
      dest <- map[i, 1] + (input - map[i, 2])
      break
    }

    i <- i + 1L
  }

  # No hits means its 1 -> 1 map
  if (!length(dest)) {
    dest <- input
  }

  return(dest)
}

# Given our start values
# run each value through our maps iteratively until
# we end up with a final value
reduce_maps <- function(init, maps) {

  out <- init

  for (map in maps) {
    out <- vapply(out, eval_source_in_map, numeric(1), map)
  }

  return(out)
}

# === Get seeds ===================================================
seeds <- sub("seeds: ", "", raw_data[1]) |>
  strsplit(" ") |>
  unlist() |>
  as.numeric()
  

# === Returns a list of matrices ====================================
# which is a structured representation of the data
maps <- build_maps(raw_data)

locations <- reduce_maps(seeds, maps)

# min location
min(locations)
# 31599214
