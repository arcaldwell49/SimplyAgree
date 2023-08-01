# SimplyAgree Update: v0.2.0
Aaron R. Caldwell

# Welcome to version 0.2.0 (July 2023)

I am happy to announce that an early release of the SimplyAgree. As
always, detailed documentation of the package can be found the
[website](https://aaroncaldwell.us/SimplyAgree/). This update mainly
about two new functions: `tolerance_limit` and `agreement_limit`.

# Rationale

I have never really liked that agreement limits are calculated from 3
different functions so I have created 1 function, `agreement_limit`, to
calculate the 3 main types of agreement limits included in the package.
Additionally, users will have the option of using Bland-Altman style
confidence limits or Zou’s MOVER confidence limits.

The tolerance limits are inspired the recent work of Francq[1]. The
tolerance limit is essentially a confidence limit on a prediction
interval of the difference scores. While the concept might *sound*
confusing the average user can just consider the prediction interval to
be an equivalent of the agreement limit and the tolerance limit to be
the equivalent of the confidence limits for agreement limits. This is
advantageous for 2 reasons:

1.  Tolerance and prediction intervals often have better coverage
2.  Tolerance limits can be directly calculated from the estimated
    marginal means from a generalized least squares (GLS) model and are
    therefore more flexible

# Agreement Limits

The agreement limits function is very simliar to the other “agree”
functions in the package.

``` r
library(SimplyAgree)
data('reps')

# Simple
agreement_limit(x = "x", y ="y", data = reps)
```

    MOVER Limits of Agreement (LoA)
    95% LoA @ 5% Alpha-Level
    Independent Data Points

       Bias           Bias CI Lower LoA Upper LoA            LoA CI
     0.4383 [-0.1669, 1.0436]    -1.947     2.824 [-3.0117, 3.8884]

    SD of Differences = 1.217

``` r
# Replicates
agreement_limit(x = "x", y ="y", data = reps, id = "id", data_type = "rep")
```

    MOVER Limits of Agreement (LoA)
    95% LoA @ 5% Alpha-Level
    Data with Replicates

       Bias           Bias CI Lower LoA Upper LoA            LoA CI
     0.7152 [-1.5287, 2.9591]    -2.232     3.662 [-7.5482, 8.9786]

    SD of Differences = 1.5036

``` r
# Nested
agreement_limit(x = "x", y ="y", data = reps, id = "id", data_type = "nest")
```

    MOVER Limits of Agreement (LoA)
    95% LoA @ 5% Alpha-Level
    Nested Data

       Bias           Bias CI Lower LoA Upper LoA            LoA CI
     0.7046 [-1.5572, 2.9664]    -2.153     3.562 [-7.4979, 8.9071]

    SD of Differences = 1.4581

# Tolerance Limit

The tolerance limit function operates in a similar fashion. The only
difference is that it cannot handle data with replicates explicitly.

``` r
data('reps')

# Simple
tolerance_limit(x = "x", y ="y", data = reps)
```

    Agreement between Measures (Difference: x-y)
    95% Prediction Interval with 95% Tolerance Limits

       Bias           Bias CI Prediction Interval Tolerance Limits
     0.4383 [-0.1669, 1.0436]   [-2.1998, 3.0764] [-2.993, 3.8697]

``` r
# Nested
tolerance_limit(x = "x", y ="y", data = reps, id = "id")
```

    Agreement between Measures (Difference: x-y)
    95% Prediction Interval with 95% Tolerance Limits

       Bias           Bias CI Prediction Interval   Tolerance Limits
     0.7046 [-1.5571, 2.9663]   [-4.4686, 5.8778] [-8.6099, 10.0191]

# Log transformation

Additionally, users can set the argument `log_tf` to TRUE, and this will
log-transform the raw data. This can be advantageous in situations where
the error is proportional to the mean (heteroscedascity). The
interpretation is slightly different since the data is on the log-scale.
When back transformed the differences between the two measures is on the
ratio scale (e.g., x/y versus x-y).

``` r
tolerance_limit(x = "x", y ="y", 
                data = reps, id = "id",
                log_tf = TRUE)
```

    Agreement between Measures (Ratio: x/y)
    95% Prediction Interval with 95% Tolerance Limits

      Bias          Bias CI Prediction Interval Tolerance Limits
     1.117 [0.7251, 1.7202]    [0.4162, 2.9969] [0.1888, 6.6065]

# Concluding Remarks

It has taken about half of this year to get tolerance limits into the
package. I believe this function will greatly improve the ease of
calculating agreement/tolerance limits. Just like the other agree
functions there is a `check` and `plot` functions/methods so you can
check your assumptions and plot your data.

If you have any questions, concerns, or have suggestions please feel
free to reach out. I have a “Contact Me” form on my
[website](https://aaroncaldwell.us/#contact), and feel free to send a
message at any time. I would appreciate any feedback.

I hope you all enjoy the new SimplyAgree!

Cheers everyone,

AC

[1] Francq, B. G., Berger, M., & Boachie, C. (2020). To tolerate or to
agree: A tutorial on tolerance intervals in method comparison studies
with BivRegBLS R Package. Statistics in Medicine, 39(28), 4334-4349.
