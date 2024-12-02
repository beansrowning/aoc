# For math helpers
source("R/util.R")

# === Load data ==============================================
data_pull <- function(lookup, path = "08/data") {
  con <- file(path, "r")
  on.exit(close(con))

  raw_text <- readLines(con)

  instructions <- strsplit(raw_text[1], "")[[1]]

  reg <- "([A-Z]{3}) = \\(([A-Z]{3})\\, ([A-Z]{3})\\)"

  # Extract our three components of the graph
  split_text <- regmatches(raw_text[-1:-2], gregexec(reg, raw_text[-1:-2]))

  nodes <- vapply(split_text, \(x) x[2, 1], character(1))

  graph <- vapply(split_text, \(x) x[3:4, ], character(2)) |>
    array(dim = c(1, 2, length(split_text)), dimnames = list(NULL, c("L", "R"), nodes))


  return(list(instructions = instructions, graph = graph))
}

data <- data_pull()

# Based on graph viz, each A->Z must be cyclic, with a deterministic cycle length
# So we can just determine cycle length and take LCM
determine_cycle_lengths <- function(instructions, graph) {
  
  start_nodes <- dimnames(graph)[[3]][grepl("A$", dimnames(graph)[[3]])]
  current_nodes <- start_nodes

  step_direction <- ""
  i <- rep(1L, length = length(start_nodes))
  n_instructions <- length(instructions)

  for (idx in seq_along(start_nodes)) {
      repeat {

      instruction_i <- i[idx] %% n_instructions
      if (instruction_i == 0) {
        instruction_i <- n_instructions
      }

      # Move according to our instructions, looping around, if needed
      step_direction <- instructions[instruction_i]

      # find the next node based on the current node and step direction
      current_nodes[idx] <- unname(graph[1, step_direction, current_nodes[idx]])

      if (grepl("Z$", current_nodes[idx])) {
        # If we arrived, break
        break
      }
      # if not, keep going
      i[idx] <- i[idx] + 1L
    }
  }

  # Return how many steps we took
  return(i)
}

# Determine cycle length for each graph
val <- determine_cycle_lengths(data[["instructions"]], data[["graph"]])

# Find least common multiple
# must convert to double, or int will overflow
val_lcm <- do.call(lcm, as.list(as.numeric(val)))

# Have to expand the scipen to print it all
options(scipen = 14)
val_lcm
# 16342438708751