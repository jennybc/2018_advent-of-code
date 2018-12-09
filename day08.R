#' ---
#' title: "Day 8: Memory Maneuver"
#' output: github_document
#' ---

#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)

#' <https://adventofcode.com/2018/day/8>
#'
#'
#' Definitely a day of writing R as if it were C.
#'
#' Part 1
library(testthat)

#' example
ex <- scan(what = integer(1), text = "
2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
")

get_metadata <- function(x) {
  read_node <- function() {
    if (i > length(x)) return()
    n_nodes <- x[i]; i <<- i + 1
    n_meta  <- x[i]; i <<- i + 1
    for (j in seq_len(n_nodes)) read_node()
    for (j in seq_len(n_meta)) {
      meta <<- append(meta, x[i]); i <<- i + 1
    }
  }
  meta <- integer()
  i <- 1
  read_node()
  meta
}

(y <- get_metadata(ex))
sum(y)
expect_identical(sum(y), 138L)

#' my input
day08 <-  scan("day08.txt", what = integer(1))
y <- get_metadata(day08)
length(day08)
length(y)
sum(y)
