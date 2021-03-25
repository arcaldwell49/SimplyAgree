
# This file is a generated template, your changes will not be overwritten

agreetestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "agreetestClass",
    inherit = agreetestBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            if ( !is.null(self$options$method1) && !is.null(self$options$method2) ) {
                # read the option values into shorter variable names
                method1 <- self$options$method1
                method2 <- self$options$method2

                # get the data
                data <- self$data

                # convert to appropriate type
                data[[method1]] <- jmvcore::toNumeric(data[[method1]])
                data[[method2]] <- jmvcore::toNumeric(data[[method2]])

                ciWidth = self$options$ciWidth
                agreeWidth = self$options$agreeWidth
                delta_val = self$options$testValue
                res = agree_test(x = data[[method1]],
                                 y = data[[method2]],
                                 delta = delta_val,
                                 conf.level = ciWidth,
                                 agree.level = agreeWidth)

                self$results$text$setContent(res)
            }

        })
)
