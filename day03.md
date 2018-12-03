Day 3: No Matter How You Slice It
================
jenny
Mon Dec 3 13:14:06 2018

<https://adventofcode.com/2018/day/3>

``` r
parse_claims <- function(x) {
  claims <- type.convert(do.call(rbind, strsplit(x, "\\s*[^0-9]+\\s*")))
  claims <- claims[, c(-1, -2)]
  colnames(claims) <- c("left", "top", "width", "height")
  claims
}

## sets up 3-dimensional array
##   * rows * columns have same physical meaning as AoC example
##   * 3rd dimension corresponds to the individual "claims"
make_cube <- function(claims) {
  width <- max(claims[ , "left"] + claims[ , "width"])
  height <- max(claims[, "top"] + claims[ , "height"])
  array(FALSE, dim = c(height, width, nrow(claims)))
}

## FALSE --> TRUE for each sq inch inside a claim
block_one <- function(fabric, spec) {
  cols <- spec["left"] + seq_len(spec["width"])
  rows <- spec["top"] + seq_len(spec["height"])
  fabric[rows, cols] <- TRUE
  fabric
}

## blocks each claim in the cube
block <- function(cube, claims) {
  for(k in 1:nrow(claims)) {
    cube[ , , k] <- block_one(cube[ , , k], claims[k, , drop = TRUE])
  }
  cube
}
```

Part one

``` r
x <- c(
  "#1 @ 1,3: 4x4",
  "#2 @ 3,1: 4x4",
  "#3 @ 5,5: 2x2"
)

claims <- parse_claims(x)
cube <- block(make_cube(claims), claims)
overlap <- apply(cube, 1:2, sum)
sum(overlap > 1)
```

    ## [1] 4

``` r
## my input
x <- readLines("day03_input.txt")
claims <- parse_claims(x)
cube <- block(make_cube(claims), claims)
overlap <- apply(cube, 1:2, sum)
sum(overlap > 1)
```

    ## [1] 96569

Part two

``` r
## TRUEs here mean that square is included in exactly one claim
mask <- overlap == 1

## count square inches in common between a claim and the mask
common <- rep(0, length(x))
for (k in seq_len(length(x))) {
  common[[k]] <- sum(cube[ , , k] & mask)
}

## find claim where all squares are in the mask
area <- claims[, "width"] * claims[, "height"]
which(common == area)
```

    ## [1] 1023
