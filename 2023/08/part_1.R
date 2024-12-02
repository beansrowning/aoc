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

non_random_walk <- function(instructions, graph) {
  
  prev_node <- ""
  current_node <- "AAA"
  # I have a feeling this will be nice for debugging later
  # path <- character(0)
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
    prev_node <- current_node
    current_node <- graph[1, step_direction, prev_node]

    # append to our path vector
    # path <- c(path, current_node)

    if (current_node == "ZZZ") {
      # If we arrived, break
      break
    }
    # if not, keep going
    i <- i + 1L
  }

  # Return how many steps we took
  return(i)
}

val <- non_random_walk(data[["instructions"]], data[["graph"]])

val
# 19951