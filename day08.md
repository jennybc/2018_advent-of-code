Day 8: Memory Maneuver
================
jenny
Sun Dec 9 22:58:35 2018

<https://adventofcode.com/2018/day/8>

Definitely a day of writing R as if it were C.

Part 1

``` r
library(testthat)
```

example

``` r
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
#> [1] 10 11 12 99  2  1  1  2
sum(y)
#> [1] 138
expect_identical(sum(y), 138L)
```

my input

``` r
day08 <-  scan("day08.txt", what = integer(1))
y <- get_metadata(day08)
length(day08)
#> [1] 16929
length(y)
#> [1] 13263
sum(y)
#> [1] 43351
```

part 2

``` r
get_value <- function(x) {
  read_node <- function() {
    if (i > length(x)) return()
    n_nodes <- x[i]; i <<- i + 1
    n_meta  <- x[i]; i <<- i + 1

    node_values <- integer()
    for (j in seq_len(n_nodes)) {
      node_values <- append(node_values, read_node())
    }

    meta_values <- integer()
    for (j in seq_len(n_meta)) {
      meta_values <- append(meta_values, x[i]); i <<- i + 1
    }

    if (n_nodes == 0) {
      sum(meta_values)
    } else {
      sum(node_values[meta_values], na.rm = TRUE)
    }
  }
  i <- 1
  read_node()
}

get_value(ex)
#> [1] 66

get_value(day08)
#> [1] 21502
```
