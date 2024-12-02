# Ok, it's a standard library though
library(parallel)

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
      # found
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
seed_lines <- sub("seeds: ", "", raw_data[1]) |>
  strsplit(" ") |>
  unlist() |>
  as.numeric()
  
seed_range_start <- seed_lines[seq(1, length(seed_lines), by = 2)]
seed_range_spans <- seed_lines[seq(2, length(seed_lines), by = 2)]
seed_range_ends <- seed_range_start + seed_range_spans

# === Returns a list of matrices ====================================
# which is a structured representation of the data
maps <- build_maps(raw_data)

# === Probabalistic approach ========================================
# LOL I guess with a search space over 3T and weak programming skills
# this will have to do.

seed_search <- function(seed_start, seed_end, map_list = maps, trials = 5e4, sample_size = 1e5, saddle_point_timeout = 100) {
  
  current_best_location <- 20358613
  current_best_input <- 22956591
  i <- 1
  rounds_without_improvement <- 0L

  while (i <= trials) {
    # Some running data
    print(
      sprintf(
        "[%s] (worker %s) Trial %d| Current best seed: %.0f Current best location: %.0f",
        Sys.time(),
        Sys.getpid(),
        i,
        current_best_input,
        current_best_location
      )
    )

    samples <- sample(seed_start:seed_end, sample_size)

    locations <- reduce_maps(samples, map_list)

    current_min <- min(locations)

    if (current_min < current_best_location) {
      rounds_without_improvement <- 0L
      current_best_location <- current_min
      current_best_input <- samples[which.min(locations)]
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

# === Searching space in parallel ========================
cluster <- makeCluster(
  length(seed_range_start),
  "PSOCK",
  outfile = "out.txt"
)

# Export globals to workers
clusterExport(cluster, c("reduce_maps", "maps", "eval_source_in_map"))

# Will this be the ticket?
optimal_seed <- tryCatch(
  clusterMap(
    cluster,
    seed_search,
    seed_start = seed_range_start,
    seed_end = seed_range_ends
  ),
  finally = stopCluster(cluster)
)

print(min(vapply(optimal_seed, \(x) x["location"], numeric(1))))
