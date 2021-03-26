
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

                pr_res = paste0("Limit of Agreement = ", res$shieh_test$prop0*100, "%",
                                "\n",
                                "alpha =", (1-res$conf.level), "|", res$conf.level*100,"% Confidence Interval",
                                "\n",
                                "\n",
                                "Shieh Test of Agreement",
                                "\n",
                                "Exact C.I.:"," [",round(res$shieh_test$lower.ci,4),", ",round(res$shieh_test$upper.ci, 4), "]",
                                "\n",
                                "\n",
                                "Hypothesis Test: ",res$shieh_test$h0_test)

                self$results$text$setContent(pr_res)
                table1 <- self$results$blandtab
                table1$setRow(rowNo=1, values=list(
                    var="Mean Bias",
                    estimate=res$loa$estimate[1],
                    lowerci=res$loa$lower.ci[1],
                    upperci=res$loa$upper.ci[1]
                ))

                table1$setRow(rowNo=2, values=list(
                    var="Lower Limit of Agreement",
                    estimate=res$loa$estimate[2],
                    lowerci=res$loa$lower.ci[2],
                    upperci=res$loa$upper.ci[2]
                ))
                table1$setRow(rowNo=3, values=list(
                    var="Upper Limit of Agreement",
                    estimate=res$loa$estimate[3],
                    lowerci=res$loa$lower.ci[3],
                    upperci=res$loa$upper.ci[3]
                ))

                table2 <- self$results$ccctab
                table2$setRow(rowNo=1, values=list(
                    var="CCC",
                    estimate=res$ccc.xy$est.ccc[1],
                    lowerci=res$ccc.xy$lower.ci[1],
                    upperci=res$ccc.xy$upper.ci[1]
                ))
            }

        })
)
