# Simple Agreement Analysis

Simple Agreement Analysis

## Usage

``` r
jmvagree(
  data,
  method1,
  method2,
  ciWidth = 95,
  agreeWidth = 95,
  testValue = 2,
  CCC = TRUE,
  plotbland = TRUE,
  plotcon = FALSE,
  plotcheck = FALSE,
  prop_bias = FALSE,
  xlabel = "Average of Both Methods",
  ylabel = "Difference between Methods"
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

- agreeWidth:

  a number between 50 and 99.9 (default: 95), the width of agreement
  limits

- testValue:

  a number specifying the limit of agreement

- CCC:

  `TRUE` or `FALSE` (default), produce CCC table

- plotbland:

  `TRUE` or `FALSE` (default), for Bland-Altman plot

- plotcon:

  `TRUE` or `FALSE` (default), for Bland-Altman plot

- plotcheck:

  `TRUE` or `FALSE` (default), assumptions plots

- prop_bias:

  `TRUE` or `FALSE`

- xlabel:

  The label for the x-axis on the BA plot

- ylabel:

  The label for the y-axis on the BA plot

## Value

A results object containing:

|                     |     |     |     |     |          |
|---------------------|-----|-----|-----|-----|----------|
| `results$text`      |     |     |     |     | a html   |
| `results$blandtab`  |     |     |     |     | a table  |
| `results$ccctab`    |     |     |     |     | a table  |
| `results$plotba`    |     |     |     |     | an image |
| `results$plotcon`   |     |     |     |     | an image |
| `results$plotcheck` |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$blandtab$asDF`

`as.data.frame(results$blandtab)`
