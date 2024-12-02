# === Load data ==============================================
data_pull <- function(lookup, path = "14/sample") {
  con <- file(path, "r")
  on.exit(close(con))

  raw_text <- readLines(con)
  n_char <- nchar(raw_text[1])

  out <- matrix(unlist(strsplit(raw_text, "")), nrow = length(raw_text), byrow = TRUE)

  return(out)
}

data <- data_pull()

#' @param x character vector (either row or col)
#' @param imm_char character which will not move
#' @param reversed (bool) direction of tilt
tilt_axis <- function(x, imm_char = "#", reversed = FALSE) {
  if (reversed) {
    x <- rev(x)
  }

  sections_to_tilt <- strsplit(paste(x, collapse = ""), imm_char)[[1]]

  out <- lapply(strsplit(sections_to_tilt, ""), sort, decreasing = TRUE)

  # out <- paste(vapply(tilted_sections, \(x) sprintf("#%s", paste(x, collapse = "")), character(1)), collapse = "")

  return(out)
}

apply(data, 2, tilt_axis)