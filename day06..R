#' ---
#' title: "Day 6: Chronal Coordinates"
#' output: github_document
#' ---

#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)

#' <https://adventofcode.com/2018/day/6>

## extremes of beacons in row and col direction
beacon_bounds <- function(beacons) {
  list(
    row = c(min = min(beacons$row), max = max(beacons$row)),
    col = c(min = min(beacons$col), max = max(beacons$col))
  )
}

## grid of coordinates to study
make_grid <- function(beacons) {
  e <- beacon_bounds(beacons)
  w = diff(e$col) + 1
  h = diff(e$row) + 1
  cbind(
    row = rep_len(e$row["min"]:e$row["max"], length.out = w * h),
    col = rep(e$col["min"]:e$col["max"], each = h)
  )
}

##  in: c(row, col) of one coordinate
## out: c(row, col, b), b is the closest beacon
assign_coord <- function(coord, beacons) {
  md <- abs(coord["row"] - beacons$row) + abs(coord["col"] - beacons$col)
  closest <- which(md == min(md))
  c(coord, b = if (length(closest) > 1) 0 else closest)
}

## find beacons with infinite territory and drop grid rows assigned to them
mask_grid <- function(grid, beacons) {
  e <- beacon_bounds(beacons)
  on_bb <- grid[, "row"] %in% e$row | grid[, "col"] %in% e$col
  invalid_beacons <- sort(unique(c(0, y[on_bb, "b"])))
  y[!y[, "b"] %in% invalid_beacons, ]
}

#' part 1 example
x <- tibble::tribble(
  ~col, ~row,
     1,    1,
     1,    6,
     8,    3,
     3,    4,
     5,    5,
     8,    9
)

g <- make_grid(x)
y <- t(apply(g, 1, assign_coord, beacons = x))
y <- mask_grid(y, beacons = x)
sort(table(y[, "b"]))

#' part 1 my input
day6 <- readr::read_csv("day06.txt", col_names = c("row", "col"), col_types = "ii")

g <- make_grid(day6)
y <- t(apply(g, 1, assign_coord, beacons = day6))
y <- mask_grid(y, beacons = day6)
sort(table(y[, "b"]))

#' part 2

##  in: c(row, col) of one coordinate
## out: c(row, col, td), td is total distance to all beacons
td_coord <- function(coord, beacons) {
  c(
    coord,
    td = sum(abs(coord["row"] - beacons$row) + abs(coord["col"] - beacons$col))
  )
}

#' work the example
cutoff <- 32
g <- make_grid(x)
z <- t(apply(g, 1, td_coord, beacons = x))
sum(z[, "td"] < cutoff)

#' work my input
cutoff <- 10000
g <- make_grid(day6)
z <- t(apply(g, 1, td_coord, beacons = day6))
sum(z[, "td"] < cutoff)
