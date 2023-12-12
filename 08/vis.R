library(tidygraph)
library(ggraph)
library(ggplot2)
library(dplyr)

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
  nodes <- setNames(seq_along(nodes), nodes)
  
  nodes_df <- tibble(
    idx = unname(nodes),
    name = names(nodes)
  )

  graph <- lapply(split_text, \(x) expand.grid(from = x[2, 1], to = x[3:4, ], stringsAsFactors = FALSE)) |>
    bind_rows()

  graph[["from"]] <- nodes[graph[["from"]]]
  graph[["to"]] <- nodes[graph[["to"]]]

  return(list(instructions = instructions, nodes = nodes_df, edges = graph))
}

data <- data_pull()

# Label start and end points
data[["nodes"]][["terminals"]] <- case_when(
  grepl("A$", data[["nodes"]][["name"]]) ~ "Start",
  grepl("Z$", data[["nodes"]][["name"]]) ~ "End",
  TRUE ~ NA_character_
)

# Assemble into a tidy-graph data structure
graph <- tbl_graph(
  nodes = data[["nodes"]],
  edges = data[["edges"]]
)

# === Viz =========================================================
plot_out <- ggraph(graph) +
  geom_edge_link() +
  geom_node_point(
    aes(
      color = terminals,
      size = ifelse(!is.na(terminals), 1.25, 1)
    )
  ) +
  geom_node_label(
    aes(
      label = ifelse(!is.na(terminals), name, NA_character_)
    ),
    repel = TRUE
  ) +
  theme_bw() +
  scale_color_manual(
    values = c(
      Start = "#1B5E20",
      End = "#B71C1C"
    ),
    na.translate = FALSE,
    na.value = "#cccccc"
  ) +
  labs(
    title = "Visualizing Day 8 Advent of Code Graph",
    subtitle = "Starts: XXA; Ends: XXZ",
    color = ""
  ) +
  guides(size = "none") +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank()
  )

ggsave(plot_out, filename = "08/network_viz.png", width = 16, height = 9)

# === New approach ===========================================================
ends <- c("ZZZ", "JNZ", "MGZ", "JVZ", "QGZ", "VQZ")

# Split graph into subgraphs based on nodes that are ultimately
# connected to connected_to
split_graph <- function(connected_to) {
  to_idx <- data[["nodes"]][["idx"]][data[["nodes"]][["name"]] == connected_to]
  
  out <- graph |>
    mutate(d = is.finite(node_distance_to(to_idx))) |>
    to_split(d)
  
  return(out[[2]])
}

subgraphs <- lapply(ends, split_graph)

subgraph_plot <- function(subgraph) {
  ggraph(subgraph) +
  geom_edge_link() +
  geom_node_point(
    aes(
      color = terminals,
      size = ifelse(!is.na(terminals), 1.25, 1)
    )
  ) +
  geom_node_label(
    aes(
      label = ifelse(!is.na(terminals), name, NA_character_)
    ),
    repel = TRUE
  ) +
  theme_bw() +
  scale_color_manual(
    values = c(
      Start = "#1B5E20",
      End = "#B71C1C"
    ),
    na.translate = FALSE,
    na.value = "#cccccc"
  ) +
  labs(
    title = "Visualizing Day 8 of 2023 Advent of Code Graph",
    subtitle = "Starts: XXA; Ends: XXZ",
    color = ""
  ) +
  guides(size = "none") +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank()
  )
}

sub_plots <- lapply(subgraphs, subgraph_plot)

sub_plots
