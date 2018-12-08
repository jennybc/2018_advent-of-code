#' ---
#' title: "Day 5: Alchemical Reduction"
#' output: github_document
#' ---

#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)

#' <https://adventofcode.com/2018/day/5>

library(testthat)
library(purrr)

#' Part 1, recursive R solution
react_detect <- function(x) {
  (toupper(x) == toupper(dplyr::lead(x)) & x != dplyr::lead(x)) %in% TRUE
}

react_impl <- function(x) {
  r <- react_detect(x)
  s <- which(r)
  if (length(s) == 0) {
    return(if (length(x) == 0) "" else x)
  }
  react_impl(x[-(min(s) + 0:1)])
}

react <- function(x) {
  paste(react_impl(strsplit(x, "")[[1]]), collapse = "")
}

expect_identical(react("aA"),     "")
expect_identical(react("abBA"),   "")
expect_identical(react("abAB"),   "abAB")
expect_identical(react("aabAAB"), "aabAAB")

expect_identical(react("dabAcCaCBAcCcaDA"), "dabCBAcaDA")
nchar(react("dabAcCaCBAcCcaDA"))

#' Hahaha, no, doesn't work on my input with 50K characters.
x <- scan("day05.txt", what = "")
nchar(x)
## nchar(react(x))
## Error: C stack usage  7970432 is too close to the limit

#' Part 1, C++ `std::string` based solution
Rcpp::sourceCpp("day05.cpp")

expect_identical(react_cpp("aA"),     "")
expect_identical(react_cpp("abBA"),   "")
expect_identical(react_cpp("abAB"),   "abAB")
expect_identical(react_cpp("aabAAB"), "aabAAB")
expect_identical(react_cpp("dabAcCaCBAcCcaDA"), "dabCBAcaDA")

#' Now with my input.
nchar(react_cpp(x))

#' Part 2, please God let my part 1 solution be truly useful.
#'
#' What "unit types" (letters) do I even have in my input?
x_split <- strsplit(x, split = "")[[1]]
x_split %>%
  toupper() %>%
  unique() %>%
  sort()
#' Looks like the whole alphabet.
#'
#' First, reproduce the example.
example <- "dabAcCaCBAcCcaDA"
n <- letters %>%
  set_names() %>%
  map(~ gsub(.x, "", example, ignore.case = TRUE)) %>%
  map(react_cpp) %>%
  map_int(nchar)
n[which.min(n)]

#' My input.
n <- letters %>%
  set_names() %>%
  map(~ gsub(.x, "", x, ignore.case = TRUE)) %>%
  map(react_cpp) %>%
  map_int(nchar)
n[which.min(n)]

