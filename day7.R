#' ---
#' title: "Day 7: The Sum of Its Parts"
#' output: github_document
#' ---

#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)

#' <https://adventofcode.com/2018/day/7>
#'
#' Part 1
library(tidygraph)
library(ggraph)
library(tidyverse)

## input: tbl_graph
## output: named list, names are nodes names, componenets are node prereqs
get_prereqs <- function(g) {
  g %>%
  activate(nodes) %>%
  mutate(
    prereqs = local_members(mindist = 1, mode = "in"),
    prereqs = purrr::map(prereqs, ~ sort(name[.x]))
  ) %>%
  as_tibble() %>%
  deframe()
}

## remove a node from list or prereqs
remove_target <- function(l, target) lapply(l, function(z) z[z != target])

## nodes names where all prereqs are met and not 'used' yet
get_candidates <- function(l, pos) {
  names(l)[lengths(l) == 0 & pos > length(pos)]
}

## graph in, ordered node string out
steps <- function(g) {
  prereqs <- get_prereqs(g)
  n <- length(prereqs)
  pos <- setNames(rep(n + 1, n), names(prereqs))
  candidates <- get_candidates(prereqs, pos)
  i <- 1
  while(length(candidates) > 0) {
    target <- sort(candidates)[1]
    pos[target] <- i
    prereqs <- remove_target(prereqs, target)
    candidates <- get_candidates(prereqs, pos)
    i <- i + 1
  }
  paste(names(sort(pos)), collapse = "")
}

#' example
ex <- scan(what = "", sep = "\n", text = "
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
")

ex <- stringr::str_extract_all(ex, "(?<!^)[A-Z]", simplify = TRUE)
g <- as_tbl_graph(ex)

## 'sugiyama' layout: "designed for directed acyclic graphs (that is,
## hierarchies where multiple parents are allowed) it minimizes the number of
## crossing edges"
ggraph(g, layout = "sugiyama") +
  geom_node_text(aes(label = name, size = 5)) +
  geom_edge_link(arrow = arrow(length = unit(4, 'mm')),
                 start_cap = circle(3, 'mm'),
                 end_cap = circle(3, 'mm')) +
  theme_graph()

#' Reproduce example result
steps(g)

#' Work with my input
day7 <- scan("day07.txt", what = "", sep = "\n")
day7 <- stringr::str_extract_all(day7, "(?<!^)[A-Z]", simplify = TRUE)
g <- as_tbl_graph(day7)
steps(g)
