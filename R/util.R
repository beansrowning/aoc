
#' @title Greatest Common Divisor
#' I took this from a really obscure stackoverflow question
#' @param ... numeric type vectors
gcd <- function(...) { Reduce(\(x, y) ifelse(y, Recall(y, x %% y), x), list(...)) }

#' @title Least Common Multiple
#' An expansion of the above to return the LCM
#' @param ... numeric type vectors
lcm <- function(...) { Reduce(\(x, y) x * y %/% gcd(x, y), list(...)) }
