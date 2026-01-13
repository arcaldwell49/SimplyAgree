# Deming Regression

Deming Regression

## Usage

``` r
jmvdeming(
  data,
  method1,
  method2,
  ciWidth = 95,
  testValue = 1,
  plotcon = FALSE,
  plotcheck = FALSE,
  weighted = FALSE,
  xlabel = "Method: 1",
  ylabel = "Method: 2"
)
```

## Arguments

- data:

  Data

- method1:

  Name of column containing 1st Vector of data

- method2:

  Name of column containing Vector of data

- ciWidth:

  a number between 50 and 99.9 (default: 95), the width of confidence
  intervals

- testValue:

  Ratio of the two error variances. Default is 1.

- plotcon:

  `TRUE` or `FALSE` (default), for Bland-Altman plot

- plotcheck:

  `TRUE` or `FALSE` (default), assumptions plots

- weighted:

  `TRUE` or `FALSE` (default), use weighted Deming regression

- xlabel:

  The label for the x-axis (default: "Method: 1")

- ylabel:

  The label for the y-axis (default: "Method: 2")

## Value

A results object containing:

|                     |     |     |     |     |          |
|---------------------|-----|-----|-----|-----|----------|
| `results$text`      |     |     |     |     | a html   |
| `results$demtab`    |     |     |     |     | a table  |
| `results$plotcon`   |     |     |     |     | an image |
| `results$plotcheck` |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$demtab$asDF`

`as.data.frame(results$demtab)`
