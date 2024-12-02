# === Params ===================================
x_min <- 200000000000000
x_max <- 400000000000000
# === Load data ==============================================
data_pull <- function(lookup, path = "24/data") {
  con <- file(path, "r")
  on.exit(close(con))
  raw_text <- scan(con, what = list("", "", "", NULL, "", "", ""))[-4]
  
  raw_mat <- do.call(cbind, raw_text)

  out <- apply(raw_mat, 2, \(x) as.double(gsub("\\,", "", x)))

  return(out)
}

data <- data_pull()

# Return all n combinations of rows from x
# as an iterator
combn_it <- function(x, n) {
  it_env <- new.env()
  assign("idx", combn(nrow(x), n), envir = it_env)
  assign("i", 0L, envir = it_env)

  out <- function() {
    it_env$i <- it_env$i + 1L

    slice <- try(
      x[it_env$idx[, it_env$i], ],
      silent = TRUE
    )

    if (inherits(slice, "try-error")) {
      # No more iterations
      return(NULL)
    }

    dimnames(slice) <- list(
      c("ice_1", "ice_2"),
      c("px", "py", "pz", "vx", "vy", "vz")
    )

    return(slice)
  }

  return(out)
}

determine_intersection <- function(x) {

  # Set up a system of equations to solve for t1, t2
  vel <- rbind(
    c(x["ice_1", "vx"], -x["ice_2", "vx"]),
    c(x["ice_1", "vy"], -x["ice_2", "vy"])
  )

  pos <- rbind(
    x["ice_2", "px"] - x["ice_1", "px"],
    x["ice_2", "py"] - x["ice_1", "py"]
  )

  t <- try(solve(vel, pos), silent = TRUE)

  if (inherits(t, "try-error") || any(t < 0)) {
    # Fails to converge, no solution
    # or crossing in the past
    return(c(x=NA_real_, y=NA_real_))
  }

  # Solve for crossing
  out <- c(
    x = x["ice_1", "px"] + x["ice_1", "vx"] * t[1, ],
    y = x["ice_1", "py"] + x["ice_1", "vy"] * t[1, ]
  )

  return(out)
}

# === Iterate over each combination and determine where they cross ===========
it <- combn_it(data, 2)
out <- matrix(NA_real_, ncol = 2)

repeat {
  ice <- it()

  if (is.null(ice)) {
    break
  }

  out <- rbind(out, determine_intersection(ice))
}

# Slice the first row which was filler
out <- out[-1, ]

# Filter based on our parameters
filter <- !is.na(out[, "x"]) & out[, "x"] >= x_min & out[, "x"] <= x_max & out[, "y"] >= x_min & out[, "y"] <= x_max
ans <- out[filter, ]


print(nrow(ans))
# 16665