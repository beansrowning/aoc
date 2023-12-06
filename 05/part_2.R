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
seed_lines <- sub("seeds: ", "", raw_data[1]) |>
  strsplit(" ") |>
  unlist() |>
  as.numeric()
  
seed_range_start <- seed_lines[seq(1, length(seed_lines), by = 2)]
seed_range_spans <- seed_lines[seq(2, length(seed_lines), by = 2)]
seed_range_ends <- seed_range_start + seed_range_spans

seed_range_lk <- cbind(
  start = seed_range_start,
  end = seed_range_ends
)

seed_search_space <- c(min(seed_range_lk[, "start"]), max(seed_range_lk[, "end"]))

# Refine sample by discarding impossible starts in search space
keep_particle <- function(x, spans_dict) {
  i <- 0L
  keep <- FALSE

  # Sort for maximum speed
  spans_dict <- spans_dict[order(spans_dict[, "start"]), ]

  while (i < nrow(spans_dict)) {
    i <- i + 1
    # If beyond current span, go to next
    if (x > spans_dict[i, "end"]) {
      next
    }

    # reaching here it must be less than
    # the end of the range, so test if it's
    # within the range
    if (x >= spans_dict[i, "start"]) {
      # falls within a range, keep
      keep <- TRUE
    }
  }

  return(keep)
}

# === Returns a list of matrices ====================================
# which is a structured representation of the data
maps <- build_maps(raw_data)

# === Probabalistic approach ========================================
# LOL I guess with a search space over 3T and weak programming skills
# this will have to do.

seed_search <- function(seed_space, seed_range_lk, maps, trials = 1e4, sample_size = 1e5, saddle_point_timeout = 10) {
  
  current_best_location <- Inf
  current_best_input <- Inf
  i <- 1
  rounds_without_improvement <- 0L

  while (i <= trials) {
    # Some running data
    print(
      sprintf(
        "Trial %d: Current best seed %.0f Current best location %.0f",
        i,
        current_best_input,
        current_best_location
      )
    )

    samples <- sample(seed_space[1]:seed_space[2], sample_size)
    filtered_samples <- samples[vapply(samples, keep_particle, logical(1), seed_range_lk)]

    locations <- reduce_maps(filtered_samples, maps)

    current_min <- min(locations)

    if (current_min < current_best_location) {
      rounds_without_improvement <- 0L
      current_best_location <- current_min
      current_best_input <- filtered_samples[which.min(locations)]
    } else {
      rounds_without_improvement <- rounds_without_improvement + 1L
      if (rounds_without_improvement > saddle_point_timeout) {
        warning(sprintf("Likely solution or saddlepoint reached after %d rounds", i))
        break
      }
    }

    i <- i + 1
  }

  return(c("seed" = current_best_input, "location" = current_best_location))
}

# We could run this in parallel
optimal_seed <- seed_search(seed_search_space, seed_range_lk, maps, saddle_point_timeout = 50L)