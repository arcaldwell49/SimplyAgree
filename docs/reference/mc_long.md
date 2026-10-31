# Convert Wide Method Comparison Data to Long Format

**\[experimental\]**

Converts data with one column per measurement method into the long
format (one row per measurement) used by
[`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
and
[`sim_methods()`](https://aaroncaldwell.us/SimplyAgree/reference/sim_methods.md).

## Usage

``` r
mc_long(
  data,
  methods,
  id = NULL,
  replicate = NULL,
  names_to = "method",
  values_to = "y"
)
```

## Arguments

- data:

  A data frame in wide format, with one column per method.

- methods:

  Character vector naming the method columns (at least two). The order
  sets the order of the `method` factor levels; the first is the
  reference method.

- id:

  Name of the column identifying the item (subject). If `NULL`
  (default), each row is treated as a separate item and an `id` column
  is created from the row number.

- replicate:

  Name of the column identifying the replicate (measurement occasion).
  If `NULL` and some items have more than one row, a `replicate` column
  is created from the row order within each item (with a message):
  methods recorded on the same row are then treated as measured on the
  same occasion.

- names_to:

  Name of the new column holding the method names. Default is "method".

- values_to:

  Name of the new column holding the measurements. Default is "y".

## Value

A tibble with columns for the item identifier (named as in `id`, or `id`
if created), the replicate (if supplied or created), the method (a
factor with levels in the order of `methods`), and the measurement. Rows
with a missing measurement are kept.

## See also

[`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md),
[`sim_methods()`](https://aaroncaldwell.us/SimplyAgree/reference/sim_methods.md)

## Examples

``` r
data("reps")
# reps has replicate pairs of x and y measured on the same occasion
reps_long = mc_long(reps, methods = c("x", "y"), id = "id")
#> Some items have several rows; a 'replicate' column was created from the row order within each item.
head(reps_long)
#> # A tibble: 6 × 4
#>      id replicate method     y
#>   <int>     <int> <fct>  <dbl>
#> 1     1         1 x       7.83
#> 2     1         1 y       6.57
#> 3     1         2 x       7.42
#> 4     1         2 y       5.62
#> 5     1         3 x       7.89
#> 6     1         3 y       6.9 
```
