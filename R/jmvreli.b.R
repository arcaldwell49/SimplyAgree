
# This file is a generated template, your changes will not be overwritten

jmvreliClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jmvreliClass",
    inherit = jmvreliBase,
    private = list(
        .run = function() {

            if(length(self$options$get("vars")) >= 2){

            variables <- self$options$get("vars")
            dataset <- as.data.frame(select(self$data, all_of(variables)))
            icctab <- self$results$icctab
            vartab <- self$results$vartab
            plot <- self$results$plots
            cl <- self$options$get("ciWidth")/100

            # ANOVA with complete data, REML (observed-data df) otherwise
            prep <- .reli_prepare(data = dataset, col.names = variables)
            method <- if (prep$balanced) "anova" else "reml"
            run_quietly <- function(f, ...) {
                suppressWarnings(f(col.names = variables, data = dataset,
                                   method = method, conf.level = cl, ...))
            }

            # The six Shrout & Fleiss ICCs, each from icc_test()
            specs <- list(
                c("oneway", "agreement", "single", "one-way random", "Agreement", "ICC1"),
                c("twoway", "agreement", "single", "two-way random", "Agreement", "ICC2"),
                c("twoway", "consistency", "single", "two-way fixed", "Consistency", "ICC3"),
                c("oneway", "agreement", "average", "one-way random", "Avg. Agreement", "ICC1k"),
                c("twoway", "agreement", "average", "two-way random", "Avg. Agreement", "ICC2k"),
                c("twoway", "consistency", "average", "two-way fixed", "Avg. Consistency", "ICC3k"))
            iccs <- lapply(specs, function(s) {
                run_quietly(icc_test, model = s[1], type = s[2], unit = s[3])
            })
            for(i in seq_along(specs)){
                icctab$setRow(rowNo=i, values=list(
                    model = specs[[i]][4],
                    measures = specs[[i]][5],
                    type = specs[[i]][6],
                    icc = unname(iccs[[i]]$estimate),
                    lower.ci = iccs[[i]]$conf.int[1],
                    upper.ci = iccs[[i]]$conf.int[2]))
            }

            vc <- iccs[[3]]$fit$vc
            vartbl = data.frame(comp = c("ID", "Items", "Residual", "Total"),
                                variance = c(vc, sum(vc)),
                                percent = c(vc, sum(vc)) / sum(vc))
            for(i in 1:4){
                vartab$setRow(rowNo=i, values=vartbl[i,])
            }

            # Measurement error from reli_err_test() (two-way, consistency)
            fmt <- function(res, digits = 4) {
                paste0(signif(unname(res$estimate), digits), " (",
                       100 * cl, "% CI ", signif(res$conf.int[1], digits),
                       " to ", signif(res$conf.int[2], digits), ")")
            }
            errs <- lapply(c(SEM = "SEM", SEE = "SEE", SEP = "SEP"),
                           function(s) run_quietly(reli_err_test, stat = s))
            cv_scale <- if (all(prep$df$values > 0)) "log" else
                if (mean(prep$df$values) > 0) "mean" else NULL
            cv_text <- if (is.null(cv_scale)) {
                "Coefficient of Variation: not available (the mean is not positive)"
            } else {
                cv <- run_quietly(reli_err_test, scale = cv_scale)
                paste0("Coefficient of Variation (",
                       if (cv_scale == "log") "sympercent" else "% of mean",
                       "): ", fmt(cv, 3))
            }

            tex_res =   paste0(
                cv_text,
                "<br>",
                "Standard Error of Measurement (SEM): ", fmt(errs$SEM),
                "<br>",
                "Standard Error of the Estimate (SEE): ", fmt(errs$SEE),
                "<br>",
                "Standard Error of Prediction (SEP): ", fmt(errs$SEP),
                "<br><br>",
                "ICC intervals are two-sided. The SEM is the square root of ",
                "the error mean square; the SEE and SEP use ICC3 and the SD ",
                "matched to its denominator (see SimplyAgree::reli_err_test).",
                if (method == "reml") paste0(
                    "<br>Some measurements are missing: estimates are from a ",
                    "linear mixed model (REML), and the intervals use ",
                    "observed-data degrees of freedom and are approximate.") else "",
                "<br>"
            )

            plot$setState(iccs[[3]])

            #citethis = paste0(
            #    "Weir (2005). Quantifying test-retest reliability using the intraclass correlation coefficient and the SEM.
            #    The Journal of Strength & Conditioning Research, 19(1), 231-240.
            #    <https://doi.org/10.1519/15184.1>"
            #)
            #self$results$cites$setContent(citethis)

            self$results$text$setContent(tex_res)
            }

        },
        .plot=function(image, ggtheme, ...) {

            if (is.null(image$state))
                return(FALSE)

            plotpr = plot(image$state)+
            ggtheme +
              theme(legend.position = "none")
            #theme(#strip.text = element_text(face = "bold", size = 11),
            #legend.text = element_text(face = "bold", size = 11),
            #legend.title = element_text(face = "bold", size = 11),
            #axis.text.x = element_text(face = "bold", size = 11),
            #axis.text.y = element_text(face = "bold", size = 11),
            #axis.title.x = element_text(face = "bold", size = 11),
            #panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            #panel.background = element_rect(fill = "transparent",colour = NA),
            #plot.background = element_rect(fill = "transparent",colour = NA),
            #legend.background = element_rect(fill = "transparent",colour = NA),
            #legend.position = "none")


            print(plotpr)

            return(TRUE)
        })
)
