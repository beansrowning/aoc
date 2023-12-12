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

parallel_non_random_walk <- function(instructions, graph) {
  
  current_nodes <- dimnames(graph)[[3]][grepl("A$", dimnames(graph)[[3]])]
  prev_nodes <- vector("character", length(current_nodes))

  step_direction <- ""
  i <- 1L
  n_instructions <- length(instructions)

  repeat {

    instruction_i <- i %% n_instructions
    if (instruction_i == 0) {
      instruction_i <- n_instructions
    }

    # Move according to our instructions, looping around, if needed
    step_direction <- instructions[instruction_i]

    # find the next node based on the current node and step direction
    prev_nodes <- current_nodes
    current_nodes <- unname(graph[1, step_direction, prev_nodes])

    if (all(grepl("Z$", current_nodes))) {
      # If we arrived, break
      browser()
      break
    }
    # if not, keep going
    i <- i + 1L
  }

  # Return how many steps we took
  return(i)
}

val <- parallel_non_random_walk(data[["instructions"]], data[["graph"]])

val