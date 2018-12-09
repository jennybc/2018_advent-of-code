Day 7: The Sum of Its Parts
================
jenny
Sat Dec 8 20:48:02 2018

<https://adventofcode.com/2018/day/7>

Part 1

``` r
library(tidygraph)
#> 
#> Attaching package: 'tidygraph'
#> The following object is masked from 'package:stats':
#> 
#>     filter
library(ggraph)
#> Loading required package: ggplot2
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ tibble  1.4.99.9006     ✔ purrr   0.2.5.9000 
#> ✔ tidyr   0.8.2           ✔ dplyr   0.7.99.9000
#> ✔ readr   1.2.1           ✔ stringr 1.3.1      
#> ✔ tibble  1.4.99.9006     ✔ forcats 0.3.0
#> ── Conflicts ────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks tidygraph::filter(), stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()

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
```

example

``` r
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
```

![](day7_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Reproduce example result

``` r
steps(g)
#> [1] "CABDFE"
```

Work with my input

``` r
day7 <- scan("day07.txt", what = "", sep = "\n")
day7 <- stringr::str_extract_all(day7, "(?<!^)[A-Z]", simplify = TRUE)
g <- as_tbl_graph(day7)
steps(g)
#> [1] "JKNSTHCBGRVDXWAYFOQLMPZIUE"
```
