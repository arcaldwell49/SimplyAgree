
# This file is a generated template, your changes will not be overwritten

jmvreliClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jmvreliClass",
    inherit = jmvreliBase,
    private = list(
        .run = function() {

            if(length(self$options$get("vars")) >= 2){

            variables <- self$options$get("vars")
            dataset <- select(self$data, variables)
            icctab <- self$results$icctab
            vartab <- self$results$vartab
            plot <- self$results$plots
            cl <- self$options$get("ciWidth")/100

            res = reli_stats(data = dataset,
                             wide = TRUE,
                             col.names = variables,
                             conf.level = cl)
            for(i in 1:6){
                icctab$setRow(rowNo=i, values=res$icc[i,])
            }

            vartbl = data.frame(comp=rownames(res$var_comp),
                              variance = res$var_comp$variance,
                              percent = res$var_comp$percent)
            for(i in 1:4){
                vartab$setRow(rowNo=i, values=vartbl[i,])
            }

            tex_res =   paste0(
                "Coefficient of Variation (%): ",
                round(res$cv$est*100,2),
                "\n",
                "Standard Error of Measurement (SEM): ",
                round(res$SEM$est,4),
                "\n",
                "Standard Error of the Estimate (SEE): ",
                round(res$SEE$est,4),
                "\n",
                "Standard Error of Prediction (SEP): ",
                round(res$SEP$est,4),
                "\n"
            )

            plot$setState(res)

            #citethis = paste0(
            #    "Weir (2005). Quantifying test-retest reliability using the intraclass correlation coefficient and the SEM.
            #    The Journal of Strength & Conditioning Research, 19(1), 231-240.
            #    <https://doi.org/10.1519/15184.1>"
            #)
            #self$results$cites$setContent(citethis)

            self$results$text$setContent(tex_res)
            }

        },
        .plot=function(image, ...) {

            if (is.null(image$state))
                return(FALSE)

            plotpr = plot(image$state)


            print(plotpr)

            return(TRUE)
        })
)
