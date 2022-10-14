
# This file is a generated template, your changes will not be overwritten

jmvagreeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jmvagreeClass",
    inherit = jmvagreeBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            if ( !is.null(self$options$method1) && !is.null(self$options$method2) ) {
                # read the option values into shorter variable names
                method1 <- self$options$method1
                method2 <- self$options$method2

                plotba <- self$results$plotba
                plotcon <- self$results$plotcon
                plotcheck <- self$results$plotcheck

                # get the data
                data <- self$data

                # convert to appropriate type
                data[[method1]] <- jmvcore::toNumeric(data[[method1]])
                data[[method2]] <- jmvcore::toNumeric(data[[method2]])

                ciWidth = self$options$ciWidth/100
                agreeWidth = self$options$agreeWidth/100
                prop_bias = self$options$prop_bias
                delta_val = self$options$testValue
                res = agree_test(x = data[[method1]],
                                 y = data[[method2]],
                                 delta = delta_val,
                                 conf.level = ciWidth,
                                 agree.level = agreeWidth,
                                 prop_bias = prop_bias)
                res$call$conf.level = ciWidth
                res$call$agree.level = agreeWidth
                res$call$delta = delta_val
                res$call$prop_bias = prop_bias

                pr_res1 = res$call
                exact_ci = ifelse(res$call$TOST,
                                  1-((1-res$call$conf.level)*2),
                                  res$call$conf.level)
                if(res$call$prop_bias){
                  pbias_txt = "&#x1f6a8 Warning: hypothesis test likely bogus with proportional bias. <br>"
                } else {
                  pbias_txt = "<br>"
                }
                if(grepl(res$shieh_test$h0_test, pattern="don't")){
                  h0_text = "&#10060"
                } else{
                  h0_text = "&#9989"

                }
                pr_ci = paste0(ciWidth*100, "% (Bias), ",(1-(1-ciWidth)*2)*100, "% (LoA)")
                pr_res = paste0(
                  pbias_txt,
                  "Shieh Test of Agreement ",
                  "<br>",
                  "Exact ", exact_ci*100,"% C.I.:",
                  " [",
                  round(res$shieh_test$lower.ci, 4),
                  ", ",
                  round(res$shieh_test$upper.ci, 4),
                  "]",
                  "<br>",
                  "<br>", h0_text,
                  " Hypothesis Test: ",
                  res$shieh_test$h0_test,
                  "<br>",
                  "<br>",
                  "Limit of Agreement = ",
                  res$shieh_test$prop0 * 100,
                  "%",
                  "<br>",
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

                plotba$setState(res)
                plotcon$setState(res)
                plotcheck$setState(res)
            }



            #citethis = paste0(
            #    "Shieh (2019). Assessing Agreement Between Two Methods of Quantitative Measurements: Exact Test Procedure and Sample Size Calculation,
            #    Statistics in Biopharmaceutical Research,
            #    <https://doi.org/10.1080/19466315.2019.1677495>"
            #)
            #self$results$cites$setContent(citethis)
        },
        .plotba = function(image,ggtheme,...){

            if (is.null(image$state))
                return(FALSE)

            plotpr = plot(image$state,
                          x_name = "Method 1", y_name = "Method 2") +
              labs(y = self$options$ylabel,
                   x = self$options$xlabel) +
                 #set transparency
              ggtheme+
              theme(legend.title=element_blank())
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

            plotpr = plot(image$state, type = 2,
                          x_name = "1", y_name = "2") +
              ggtheme +
              theme(legend.title=element_blank())
                # set transparency
                #theme(
                #    panel.grid.major = element_blank(),
                #    panel.grid.minor = element_blank(),
                #    legend.key = element_rect(colour = "transparent", fill = "transparent"),
                #    panel.background = element_rect(fill = "transparent",colour = NA),
                #    plot.background = element_rect(fill = "transparent",colour = NA)
                #)


            print(plotpr)

            return(TRUE)

        },
        .plotcheck = function(image, ggtheme,...){

          if (is.null(image$state))
            return(FALSE)

          plotpr = check(image$state) #+
            # set transparency
            #ggtheme
            #theme(strip.text = element_text(face = "bold", size = 11),
            #          legend.text = element_text(face = "bold", size = 11),
            #          legend.title = element_text(face = "bold", size = 11),
            #          axis.text.x = element_text(face = "bold", size = 11),
            #          axis.text.y = element_text(face = "bold", size = 11),
            #          axis.title.x = element_text(face = "bold", size = 11),
            #          panel.grid.major = element_blank(),
            #          panel.grid.minor = element_blank(),
            #          panel.background = element_rect(fill = "transparent",colour = NA),
            #          plot.background = element_rect(fill = "transparent",colour = NA),
            #          legend.background = element_rect(fill = "transparent",colour = NA))



          print(plotpr)

          return(TRUE)

        })
)
