Day 1: Chronal Calibration
================
jenny
Sun Dec 2 22:16:51 2018

<https://adventofcode.com/2018/day/1>

``` r
library(testthat)

frequency <- function(deltas = 0, initial = 0) {
  sum(c(initial, deltas))
}

expect_identical(frequency(c(1, -2, 3, 1)), 3)
expect_identical(frequency(c(1, 1, 1)), 3)
expect_identical(frequency(c(1, 1, -2)), 0)
expect_identical(frequency(c(-1, -2, -3)), -6)

x <- readLines("day01_input.txt")
x <- readr::parse_integer(x)
frequency(x)
```

    ## [1] 547

``` r
dupfreq <- function(deltas = 0, initial = 0, n = 10) {
  x <- c(initial, rep(deltas, times = n))
  y <- cumsum(x)
  y[min(which(duplicated(y)))]
}

expect_identical(dupfreq(c(1, -2, 3, 1)), 2)
expect_identical(dupfreq(c(1, -1)), 0)
expect_identical(dupfreq(c(3, 3, 4, -2, -4)), 10)
expect_identical(dupfreq(c(-6, 3, 8, 5, -6)), 5)
expect_identical(dupfreq(c(7, 7, -2, -7, -4)), 14)

dupfreq(x, n = 1000)
```

    ## [1] 76414
