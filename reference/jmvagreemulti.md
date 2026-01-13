# Nested/Replicate Data Agreement Analysis

Nested/Replicate Data Agreement Analysis

## Usage

``` r
jmvagreemulti(
  data,
  method1,
  method2,
  id,
  ciWidth = 95,
  agreeWidth = 95,
  testValue = 2,
  CCC = TRUE,
  valEq = FALSE,
  plotbland = FALSE,
  plotcon = FALSE,
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

- id:

  Name of column containing subject identifier

- ciWidth:

  a number between 50 and 99.9 (default: 95), the width of confidence
  intervals

- agreeWidth:

  a number between 50 and 99.9 (default: 95), the width of agreement
  limits

- testValue:

  a number specifying the limit of agreement

- CCC:

  `TRUE` (default) or `FALSE`, produce CCC table

- valEq:

  .

- plotbland:

  `TRUE` or `FALSE` (default), for Bland-Altman plot

- plotcon:

  `TRUE` or `FALSE` (default), for Line of identity plot

- prop_bias:

  `TRUE` or `FALSE` (default), proportional bias

- xlabel:

  The label for the x-axis on the BA plot (default: "Average of Both
  Methods")

- ylabel:

  The label for the y-axis on the BA plot (default: "Difference between
  Methods")

## Value

A results object containing:

|                    |     |     |     |     |                |
|--------------------|-----|-----|-----|-----|----------------|
| `results$text`     |     |     |     |     | a preformatted |
| `results$blandtab` |     |     |     |     | a table        |
| `results$ccctab`   |     |     |     |     | a table        |
| `results$plotba`   |     |     |     |     | an image       |
| `results$plotcon`  |     |     |     |     | an image       |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$blandtab$asDF`

`as.data.frame(results$blandtab)`
