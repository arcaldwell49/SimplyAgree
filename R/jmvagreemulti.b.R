
# This file is a generated template, your changes will not be overwritten

jmvagreemultiClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jmvagreemultiClass",
    inherit = jmvagreemultiBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            if ( !is.null(self$options$method1) && !is.null(self$options$method2) && !is.null(self$options$id) ) {
                # read the option values into shorter variable names
                method1 <- self$options$method1
                method2 <- self$options$method2
                id <- self$options$id

                # get the data
                data <- self$data

                # convert to appropriate type
                #data[[method1]] <- jmvcore::toNumeric(data[[method1]])
                #data[[method2]] <- jmvcore::toNumeric(data[[method2]])
                #data[[id]] <- data[[id]]

                ciWidth = self$options$ciWidth
                agreeWidth = self$options$agreeWidth
                delta_val = jmvcore::toNumeric(self$options$testValue)
                if(self$options$valEq){

                    res = agree_reps(
                        x = method1,
                        y = method2,
                        id = id,
                        data = data,
                        delta = delta_val,
                        conf.level = ciWidth,
                        agree.level = agreeWidth
                    )

                } else {

                    res = agree_nest(
                        x = method1,
                        y = method2,
                        id = id,
                        data = data,
                        delta = delta_val,
                        conf.level = ciWidth,
                        agree.level = agreeWidth
                    )

                }

                self$results$text$setContent(res)
            }

        })
)
