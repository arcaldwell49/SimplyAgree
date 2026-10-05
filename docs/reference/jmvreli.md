# Reliability Analysis

Reliability Analysis

## Usage

``` r
jmvreli(data, vars, ciWidth = 95, desc = FALSE, plots = FALSE)
```

## Arguments

- data:

  the data as a data frame

- vars:

  a list of the column names containing the measurements for reliability
  analysis.

- ciWidth:

  a number between 50 and 99.9 (default: 95), the width of confidence
  intervals

- desc:

  `TRUE` or `FALSE` (default), provide table of variance components

- plots:

  `TRUE` or `FALSE` (default), plot data

## Value

A results object containing:

|                  |     |     |     |     |          |
|------------------|-----|-----|-----|-----|----------|
| `results$text`   |     |     |     |     | a html   |
| `results$icctab` |     |     |     |     | a table  |
| `results$vartab` |     |     |     |     | a table  |
| `results$plots`  |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$icctab$asDF`

`as.data.frame(results$icctab)`
