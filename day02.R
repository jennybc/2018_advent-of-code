#' ---
#' title: "Day 2: Inventory Management System"
#' output: github_document
#' ---

#' <https://adventofcode.com/2018/day/2>

library(testthat)
library(purrr)

# abcdef contains no letters that appear exactly two or three times.
# bababc contains two a and three b, so it counts for both.
# abbcde contains two b, but no letter appears exactly three times.
# abcccd contains three c, but no letter appears exactly two times.
# aabcdd contains two a and two d, but it only counts once.
# abcdee contains two e.

checksum <- function(x) {
  y <- x %>%
    strsplit(split = "") %>%
    map(table) %>%
    map(as.vector)
  sum(map_lgl(y, ~ any(.x == 2))) * sum(map_lgl(y, ~ any(.x == 3)))
}

x <- c("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
expect_identical(checksum(x), 12L)

x <- readLines("day02_input.txt")
checksum(x)

library(stringdist)

sub_one <- function(x) {
  y <- stringdistmatrix(x, method = "hamming")
  z <- as.matrix(y)
  this <- which(z == 1)
  these <- sort(this %% length(x))
  x[these]
}

common_letters <- function(p) {
  q <- strsplit(p, split = "")
  mismatch <- q[[1]] != q[[2]]
  paste0(q[[1]][!mismatch], collapse = "")
}

x <- c("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")
expect_identical(
  x %>%
    sub_one() %>%
    common_letters(),
  "fgij"
)

x <- readLines("day02_input.txt")
x %>%
  sub_one() %>%
  common_letters()
