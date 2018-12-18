Day 6: Chronal Coordinates
================
jenny
Tue Dec 18 12:17:01 2018

<https://adventofcode.com/2018/day/6>

``` r
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
```

part 1 example

``` r
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
#> 
#>  4  5 
#>  9 17
```

part 1 my
input

``` r
day6 <- readr::read_csv("day06.txt", col_names = c("row", "col"), col_types = "ii")

g <- make_grid(day6)
y <- t(apply(g, 1, assign_coord, beacons = day6))
y <- mask_grid(y, beacons = day6)
sort(table(y[, "b"]))
#> 
#>   14   23   49   32   26   47    2   15   43   24   38   40   21   20   45 
#>  816 1102 1167 1358 1366 1485 1528 1638 1647 1682 1748 1781 1908 2200 2257 
#>   48   50   25   34   17    8    1   33   39   46   28 
#> 2504 2568 2594 2638 2761 2779 2785 3003 3217 3750 4143
```

part 2

``` r
##  in: c(row, col) of one coordinate
## out: c(row, col, td), td is total distance to all beacons
td_coord <- function(coord, beacons) {
  c(
    coord,
    td = sum(abs(coord["row"] - beacons$row) + abs(coord["col"] - beacons$col))
  )
}
```

work the example

``` r
cutoff <- 32
g <- make_grid(x)
z <- t(apply(g, 1, td_coord, beacons = x))
sum(z[, "td"] < cutoff)
#> [1] 16
```

work my input

``` r
cutoff <- 10000
g <- make_grid(day6)
z <- t(apply(g, 1, td_coord, beacons = day6))
sum(z[, "td"] < cutoff)
#> [1] 35039
```
