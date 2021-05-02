
# This file is a generated template, your changes will not be overwritten

jmvreliClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jmvreliClass",
    inherit = jmvreliBase,
    private = list(
        .run = function() {

            variables <- self$options$get("vars")

            dataset <- select(self$data, variables)
            icctab <- self$results$icctab
            cl <- self$options$get("ciWidth")

            res = reli_stats(data = dataset,
                             wide = TRUE,
                             col.names = variables,
                             conf.level = cl)
            for(i in 1:6){
                icctab$setRow(rowNo=i, values=res$icc[i,])
            }



            self$results$text$setContent(res)

        })
)
