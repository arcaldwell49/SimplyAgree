
# This file is a generated template, your changes will not be overwritten

jmvdemingClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jmvdemingClass",
    inherit = jmvdemingBase,
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
          df = data
          colnames(df) = c("method1", "method2")

          plotcon <- self$results$plotcon
          plotcheck <- self$results$plotcheck



          ciWidth = self$options$ciWidth/100

          delta_val = self$options$testValue
          res = dem_reg(x = "method1",
                        y = "method2",
                        data = df,
                        error.ratio = delta_val,
                        weighted = self$options$weighted,
                        conf.level = ciWidth)
          res$call$conf.level = ciWidth
          res$call$weighted = self$options$weighted
          #res$call$agree.level = agreeWidth
          #res$call$delta = delta_val
          #res$call$prop_bias = prop_bias

          pr_res1 = res$call
          if(self$options$weighted){
            pr_res = paste0("Weighted Deming Regression with ", self$options$ciWidth, "% Confidence Intervals")
          } else {
            pr_res = paste0("Deming Regression with ", self$options$ciWidth, "% Confidence Intervals")
          }

          self$results$text$setContent(pr_res)
          table1 <- self$results$demtab
          table1$setRow(rowNo=1, values=list(
            var="Intercept",
            estimate=res$model$coef[1],
            se = res$model$se[1],
            df = res$model$df[1],
            lowerci=res$model$lower.ci[1],
            upperci=res$model$upper.ci[1]
          ))

          table1$setRow(rowNo=2, values=list(
            var="Slope",
            estimate=res$model$coef[2],
            se = res$model$se[2],
            df = res$model$df[2],
            lowerci=res$model$lower.ci[2],
            upperci=res$model$upper.ci[2]
          ))

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
      .plotcon = function(image,...){

        if (is.null(image$state))
          return(FALSE)

        plotpr = plot(image$state) +
          labs(x = self$options$xlabel,
               y = self$options$ylabel)
          # set transparency
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)
          )


        print(plotpr)

        return(TRUE)

      },
      .plotcheck = function(image,...){

        if (is.null(image$state))
          return(FALSE)

        plotpr = check(image$state) +
          # set transparency
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)
          )


        print(plotpr)

        return(TRUE)

      })
)
