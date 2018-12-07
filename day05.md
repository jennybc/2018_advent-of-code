Day 5: Alchemical Reduction
================
jenny
Fri Dec 7 10:57:29 2018

<https://adventofcode.com/2018/day/5>

``` r
library(testthat)
```

Part 1, recursive R solution

``` r
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
#> [1] 10
```

Hahaha, no, doesn’t work on my input with 50K characters.

``` r
x <- scan("day05.txt", what = "")
nchar(x)
#> [1] 50000
## nchar(react(x))
## Error: C stack usage  7970432 is too close to the limit
```

Part 12, C++ string based solution

``` r
Rcpp::sourceCpp("day05.cpp")

expect_identical(react_cpp("aA"),     "")
expect_identical(react_cpp("abBA"),   "")
expect_identical(react_cpp("abAB"),   "abAB")
expect_identical(react_cpp("aabAAB"), "aabAAB")
expect_identical(react_cpp("dabAcCaCBAcCcaDA"), "dabCBAcaDA")
```

Now with my input.

``` r
nchar(react_cpp(x))
#> [1] 11894
```
