
# This file is a generated template, your changes will not be overwritten

jmvagreemultiClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jmvagreemultiClass",
    inherit = jmvagreemultiBase,
    private = list(
        .run = function() {


            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            if ( !is.null(self$options$method1) &&
                 !is.null(self$options$method2) &&
                 !is.null(self$options$id) ) {
                # read the option values into shorter variable names
                method1 <- self$options$method1
                method2 <- self$options$method2
                id <- self$options$id
                plotba <- self$results$plotba
                plotcon <- self$results$plotcon

                # get the data
                data <- self$data

                # convert to appropriate type
                data[[method1]] <- jmvcore::toNumeric(data[[method1]])
                data[[method2]] <- jmvcore::toNumeric(data[[method2]])
                data[[id]] <- as.factor(data[[id]])

                ciWidth = self$options$ciWidth/100
                agreeWidth = self$options$agreeWidth/100
                delta_val = jmvcore::toNumeric(self$options$testValue)
                prop_bias = self$options$prop_bias
                if(self$options$valEq){

                    res = SimplyAgree::agree_reps(
                        x = method1,
                        y = method2,
                        id = id,
                        data = data,
                        delta = delta_val,
                        conf.level = ciWidth,
                        agree.level = agreeWidth,
                        TOST = TRUE

                    )

                    res$call$conf.level = ciWidth
                    res$call$agree.level = agreeWidth
                    res$call$delta = as.numeric(delta_val)
                    res$call$id = "id"
                    res$call$x = "method1"
                    res$call$y = "method2"
                    res$call$data = as.data.frame(data)
                    res$call$prop_bias = prop_bias
                    res$call$TOST = TRUE

                    pr_res1 = res$call
                    exact_ci = ifelse(res$call$TOST,
                                      1-((1-res$call$conf.level)*2),
                                      res$call$conf.level)
                    if(res$call$prop_bias){
                      pbias_txt = "Warning: agreement limits at average response with proportional bias. Check plots."
                    } else {
                      pbias_txt = "\n"
                    }
                    pr_res = paste0(
                      pbias_txt,
                      "\n",
                      "Limit of Agreement = ",
                      agreeWidth * 100,
                      "%",
                      "\n",
                      "Confidence Interval = ",
                      #"alpha =", (1-res$call$conf.level),   "|",
                      ciWidth*100,
                      "%"
                    )

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

                } else {

                    res = SimplyAgree::agree_nest(
                        x = method1,
                        y = method2,
                        id = id,
                        data = data,
                        delta = delta_val,
                        conf.level = ciWidth,
                        agree.level = agreeWidth,
                        TOST = TRUE
                    )

                    res$call$conf.level = ciWidth
                    res$call$agree.level = agreeWidth
                    res$call$delta = as.numeric(delta_val)
                    res$call$data = as.data.frame(data)
                    res$call$prop_bias = prop_bias
                    res$call$id = "id"
                    res$call$x = "method1"
                    res$call$y = "method2"
                    pr_ci = paste0(ciWidth*100, "% (Bias), ",(1-(1-ciWidth)*2)*100, "% (LoA)")

                    pr_res1 = res$call
                    exact_ci = ifelse(res$call$TOST,
                                      1-((1-res$call$conf.level)*2),
                                      res$call$conf.level)
                    if(res$call$prop_bias){
                      pbias_txt = "Warning: agreement limits at average response with proportional bias. Check plots. \n"
                    } else {
                      pbias_txt = "\n"
                    }
                    pr_res = paste0(
                      pbias_txt,
                      "\n",
                      "Limit of Agreement = ",
                      agreeWidth * 100,
                      "%",
                      "\n",
                      "Confidence Interval = ",
                      #"alpha =", (1-res$call$conf.level),   "|",
                      pr_ci
                    )

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

                plotba$setState(res)
                plotcon$setState(res)

                #citethis = paste0(
                #    "Zou (2013). Confidence interval estimation for the Bland Altman limits of agreement with multiple observations per individual,
                #Statistical methods in medical research,
                #<https://doi.org/10.1177/0962280211402548>",
                #"\n",
                #"Carrasco et al (2013). Estimation of the concordance correlation coefficient for repeated measures using SAS and R,
                #Computer Methods and Programs in Biomedicine,
                #<https://doi.org/10.1016/j.cmpb.2012.09.002>"
                #)
                #self$results$cites$setContent(citethis)

                self$results$text$setContent(pr_res)
            }

        },
        .plotba = function(image, ggtheme,...){

            if (is.null(image$state))
                return(FALSE)

            plotpr = plot(image$state,
                          x_name = "Method 1", y_name = "Method 2") +
              labs(y = self$options$ylabel,
                   x = self$options$xlabel) +
              ggtheme
                # set transparency
                #theme(
                #    panel.grid.major = element_blank(),
                #    panel.grid.minor = element_blank(),
                #    legend.background = element_rect(fill = "transparent",colour = NA),
                #    panel.background = element_rect(fill = "transparent",colour = NA),
                #    plot.background = element_rect(fill = "transparent",colour = NA)
                #)


            print(plotpr)

            return(TRUE)

        },
        .plotcon = function(image,ggtheme,...){

            if (is.null(image$state))
                return(FALSE)

            plotpr = plot(image$state, type =2,
                          x_name = "1", y_name = "2")+
              ggtheme
                # set transparency
                #theme(
                #    panel.grid.major = element_blank(),
                #    panel.grid.minor = element_blank(),
                #    panel.background = element_rect(fill = "transparent",colour = NA),
                #    plot.background = element_rect(fill = "transparent",colour = NA)
                #)


            print(plotpr)

            return(TRUE)

        })
)
