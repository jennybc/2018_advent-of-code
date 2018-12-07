Day 4: Repose Record
================
jenny
Thu Dec 6 22:41:08 2018

<https://adventofcode.com/2018/day/4>

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.1.0           ✔ purrr   0.2.5.9000 
#> ✔ tibble  1.4.99.9006     ✔ dplyr   0.7.99.9000
#> ✔ tidyr   0.8.2           ✔ stringr 1.3.1      
#> ✔ readr   1.2.1           ✔ forcats 0.3.0
#> ── Conflicts ────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()

x <- scan(what = "", sep = "\n", text = "
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
")

get_field <- function(string, pattern) {
  str_match(string, pattern)[ , 2, drop = TRUE]
}

parse_log <- function(x) {
  tibble(x) %>%
    mutate(
      guard = as.integer(get_field(x, "#([0-9]+)")),
      minute = as.integer(get_field(x, ":([0-9]+)")),
      event = get_field(x, "\\] ([a-z ]+)$"),
      shift = cumsum(event %in% "falls asleep")
    ) %>%
    fill(guard) %>%
    filter(!is.na(event)) %>%
    select(-x) %>%
    spread(key = event, value = minute) %>%
    as_tibble(.name_repair = "universal")
}

sleepy_guard <- function(log) {
  y <- log %>%
    mutate(nap = wakes.up - falls.asleep) %>%
    count(guard, wt = nap)
  y$guard[which.max(y$n)]
}

sleepiest_minute <- function(log) {
  map2(log$falls.asleep, log$wakes.up - 1, seq) %>%
    flatten_int() %>%
    as.character() %>%
    fct_count(sort = TRUE) %>%
    rename(minute = f) %>%
    filter(n >= max(n)) %>%
    mutate(minute = as.character(minute) %>% as.integer())
}

aoc <- function(guard_chr, minute_df) {
  stopifnot(length(guard_chr) == 1, nrow(minute_df) == 1)
  guard * minute_df$minute
}

## part 1 puzzle statement
log <- parse_log(x)
#> New names:
#> * `falls asleep` -> falls.asleep
#> * `wakes up` -> wakes.up
(guard <- sleepy_guard(log))
#> [1] 10
(minute <- sleepiest_minute(filter(log, guard == !!guard)))
#> # A tibble: 1 x 2
#>   minute     n
#>    <int> <int>
#> 1     24     2
aoc(guard, minute)
#> [1] 240

## my input
day4 <- read_lines("day04.txt") %>% sort()

log <- parse_log(day4)
#> New names:
#> * `falls asleep` -> falls.asleep
#> * `wakes up` -> wakes.up
(guard <- sleepy_guard(log))
#> [1] 73
(minute <- sleepiest_minute(filter(log, guard == !!guard)))
#> # A tibble: 1 x 2
#>   minute     n
#>    <int> <int>
#> 1     44    14
aoc(guard, minute)
#> [1] 3212

## part 2
sleepiest_minute2 <- function(log) {
  log %>%
    group_by(guard) %>%
    nest() %>%
    mutate(
      foo = map(data, sleepiest_minute),
      data = NULL
    ) %>%
    unnest() %>%
    slice(which.max(n))
}

aoc2 <- function(log_line) {
  stopifnot(nrow(log_line) == 1)
  log_line$guard * log_line$minute
}

## part 2 puzzle statement
log <- parse_log(x)
#> New names:
#> * `falls asleep` -> falls.asleep
#> * `wakes up` -> wakes.up
(answer <- sleepiest_minute2(log))
#> # A tibble: 1 x 3
#>   guard minute     n
#>   <int>  <int> <int>
#> 1    99     45     3
aoc2(answer)
#> [1] 4455

## my input
log <- parse_log(day4)
#> New names:
#> * `falls asleep` -> falls.asleep
#> * `wakes up` -> wakes.up
(answer <- sleepiest_minute2(log))
#> # A tibble: 1 x 3
#>   guard minute     n
#>   <int>  <int> <int>
#> 1   191     26    17
aoc2(answer)
#> [1] 4966
```

I’m at the dev version or in a branch for many packages. Let’s record
that. Default printing has awkward line break, hence a workaround.

``` r
x <- as.data.frame(devtools::session_info()[["packages"]])
x %>% select(loadedversion, source)
#>             loadedversion                            source
#> assertthat          0.2.0                    CRAN (R 3.5.0)
#> backports           1.1.2                    CRAN (R 3.5.0)
#> broom               0.5.0                    CRAN (R 3.5.0)
#> callr          3.0.0.9002      Github (r-lib/callr@1cbce9d)
#> cellranger          1.1.0                    CRAN (R 3.5.0)
#> cli            1.0.1.9000        Github (r-lib/cli@56538e3)
#> colorspace          1.3-2                    CRAN (R 3.5.0)
#> crayon              1.3.4                    CRAN (R 3.5.0)
#> desc                1.2.0       Github (r-lib/desc@7c12d36)
#> devtools            2.0.1                    CRAN (R 3.5.1)
#> digest             0.6.18                    CRAN (R 3.5.0)
#> dplyr         0.7.99.9000  Github (tidyverse/dplyr@cd87394)
#> evaluate             0.12                    CRAN (R 3.5.0)
#> fansi               0.4.0    Github (brodieG/fansi@ab11e9c)
#> forcats             0.3.0                    CRAN (R 3.5.0)
#> fs                  1.2.6                    CRAN (R 3.5.0)
#> ggplot2             3.1.0                    CRAN (R 3.5.1)
#> glue                1.3.0                    CRAN (R 3.5.0)
#> gtable              0.2.0                    CRAN (R 3.5.0)
#> haven               1.1.2                    CRAN (R 3.5.0)
#> hms                 0.4.2                    CRAN (R 3.5.0)
#> htmltools           0.3.6                    CRAN (R 3.5.0)
#> httr                1.3.1                    CRAN (R 3.5.0)
#> jsonlite              1.5                    CRAN (R 3.5.0)
#> knitr                1.20                    CRAN (R 3.5.0)
#> lattice           0.20-35                    CRAN (R 3.5.1)
#> lazyeval            0.2.1                    CRAN (R 3.5.0)
#> lubridate           1.7.4                    CRAN (R 3.5.0)
#> magrittr              1.5                    CRAN (R 3.5.0)
#> memoise        1.1.0.9000   Github (hadley/memoise@1650ad7)
#> modelr              0.1.2                    CRAN (R 3.5.0)
#> munsell             0.5.0                    CRAN (R 3.5.0)
#> nlme              3.1-137                    CRAN (R 3.5.1)
#> pillar         1.3.0.9001     Github (r-lib/pillar@c5bf622)
#> pkgbuild       1.0.2.9000   Github (r-lib/pkgbuild@6e4ebdf)
#> pkgconfig           2.0.2                    CRAN (R 3.5.0)
#> pkgload             1.0.2                    CRAN (R 3.5.0)
#> plyr                1.8.4                    CRAN (R 3.5.0)
#> prettyunits         1.0.2                    CRAN (R 3.5.0)
#> processx            3.2.1   Github (r-lib/processx@afbe718)
#> ps                  1.2.1                    CRAN (R 3.5.1)
#> purrr          0.2.5.9000  Github (tidyverse/purrr@240f2b8)
#> R6                  2.3.0                    CRAN (R 3.5.0)
#> Rcpp                1.0.0                    CRAN (R 3.5.0)
#> readr               1.2.1                    CRAN (R 3.5.1)
#> readxl         1.1.0.9000                             local
#> remotes        2.0.2.9000    Github (r-lib/remotes@e56a41e)
#> rlang          0.3.0.9001      Github (r-lib/rlang@05f778f)
#> rmarkdown            1.10                    CRAN (R 3.5.0)
#> rprojroot           1.3-2                    CRAN (R 3.5.0)
#> rstudioapi            0.8                    CRAN (R 3.5.0)
#> rvest               0.3.2                    CRAN (R 3.5.0)
#> scales              1.0.0                    CRAN (R 3.5.0)
#> sessioninfo         1.1.1                    CRAN (R 3.5.1)
#> stringi             1.2.4                    CRAN (R 3.5.0)
#> stringr             1.3.1                    CRAN (R 3.5.0)
#> testthat            2.0.1                    CRAN (R 3.5.0)
#> tibble        1.4.99.9006 Github (tidyverse/tibble@ddc9110)
#> tidyr               0.8.2                    CRAN (R 3.5.0)
#> tidyselect          0.2.5                    CRAN (R 3.5.0)
#> tidyverse           1.2.1                    CRAN (R 3.5.0)
#> usethis        1.4.0.9000                             local
#> utf8                1.1.4                    CRAN (R 3.5.0)
#> withr               2.1.2                    CRAN (R 3.5.0)
#> xml2                1.2.0                    CRAN (R 3.5.0)
#> yaml                2.2.0                    CRAN (R 3.5.0)
```
