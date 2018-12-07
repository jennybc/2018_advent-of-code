#' ---
#' title: "Day 4: Repose Record"
#' output: github_document
#' ---

#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)

#' <https://adventofcode.com/2018/day/4>

library(tidyverse)

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
(guard <- sleepy_guard(log))
(minute <- sleepiest_minute(filter(log, guard == !!guard)))
aoc(guard, minute)

## my input
day4 <- read_lines("day04.txt") %>% sort()

log <- parse_log(day4)
(guard <- sleepy_guard(log))
(minute <- sleepiest_minute(filter(log, guard == !!guard)))
aoc(guard, minute)

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
(answer <- sleepiest_minute2(log))
aoc2(answer)

## my input
log <- parse_log(day4)
(answer <- sleepiest_minute2(log))
aoc2(answer)

#' I'm at the dev version or in a branch for many packages. Let's record that.
#' Default printing has awkward line break, hence a workaround.
x <- as.data.frame(devtools::session_info()[["packages"]])
x %>% select(loadedversion, source)
