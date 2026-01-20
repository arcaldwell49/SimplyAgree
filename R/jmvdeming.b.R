
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
          res = dem_reg(method2 ~method1,
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
            estimate=res$model_table$coef[1],
            se = res$model_table$se[1],
            df = res$model_table$df[1],
            lowerci=res$model_table$lower.ci[1],
            upperci=res$model_table$upper.ci[1]
          ))

          table1$setRow(rowNo=2, values=list(
            var="Slope",
            estimate=res$model_table$coef[2],
            se = res$model_table$se[2],
            df = res$model_table$df[2],
            lowerci=res$model_table$lower.ci[2],
            upperci=res$model_table$upper.ci[2]
          ))

          res$plot_data <- df  # Add before plotcon$setState(res)
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
      .plotcon = function(image,ggtheme,...){

        if (is.null(image$state))
          return(FALSE)

        res <- image$state
        df <- res$plot_data

        slp <- res$coefficients[2]
        int <- res$coefficients[1]
        tmp.lm <- data.frame(the_int = int, the_slope = slp)

        scalemin <- min(c(df$method1, df$method2), na.rm = TRUE)
        scalemax <- max(c(df$method1, df$method2), na.rm = TRUE)

        plotpr <- ggplot(df, aes(x = method1, y = method2)) +
          geom_point(na.rm = TRUE) +
          geom_abline(intercept = 0, slope = 1) +
          geom_abline(data = tmp.lm,
                      aes(intercept = the_int, slope = the_slope),
                      linetype = "dashed", color = "red") +
          labs(x = self$options$xlabel, y = self$options$ylabel) +
          xlim(scalemin, scalemax) +
          ylim(scalemin, scalemax) +
          coord_fixed(ratio = 1) +
          theme_bw() +
          ggtheme

        print(plotpr)
        return(TRUE)

      },
      .plotcheck = function(image,ggtheme,...){

        if (is.null(image$state))
          return(FALSE)

        res <- image$state
        df <- res$plot_data

        # Extract model parameters
        b0 <- res$model_table$coef[1]
        b1 <- res$model_table$coef[2]
        w_i <- res$weights
        error.ratio <- res$error.ratio

        # Compute residuals (same as check.simple_eiv for Deming)
        d_i <- df$method2 - (b0 + b1 * df$method1)
        x_hat <- df$method1 + (error.ratio * b1 * d_i) / (1 + error.ratio * b1^2)
        y_hat <- df$method2 - (d_i) / (1 + error.ratio * b1^2)
        res_x <- df$method1 - x_hat
        res_y <- df$method2 - y_hat
        d_sign <- ifelse(d_i >= 0, 1, -1)
        opt_res <- d_sign * sqrt(w_i * res_x^2 + w_i * error.ratio * res_y^2)
        avg_both <- (x_hat + y_hat) / 2

        # Breusch-Pagan test for heteroskedasticity
        mod <- lm(opt_res / d_sign ~ avg_both)
        SS <- anova(mod)$"Sum Sq"
        RegSS <- sum(SS) - SS[length(SS)]
        Chisq <- RegSS / 2
        p_val_het <- pchisq(Chisq, df = 1, lower.tail = FALSE)

        # Plot 1: Homogeneity of Residuals
        df1 <- data.frame(x = avg_both, y = opt_res / d_sign)

        p1 <- ggplot(df1, aes(x = x, y = y)) +
          geom_point() +
          geom_smooth(se = TRUE, method = "loess", linewidth = 0.8,
                      color = "#3aaf85", formula = y ~ x) +
          labs(y = "|Optimized Residuals|",
               x = "Average of Both Estimated Values",
               title = "Homogeneity of Residuals",
               subtitle = "Reference line should be flat and horizontal",
               caption = paste0("Heteroskedasticity\n",
                                "Breusch-Pagan Test: p = ",
                                signif(p_val_het, 4))) +
          ggtheme

        # Plot 2: QQ Plot for normality
        dat_norm <- na.omit(data.frame(y = opt_res))
        norm_test <- shapiro.test(opt_res)

        # Create QQ plot manually (replacing plot_qq dependency)
        qq_data <- qqnorm(dat_norm$y, plot.it = FALSE)
        qq_df <- data.frame(theoretical = qq_data$x, sample = qq_data$y)

        p2 <- ggplot(qq_df, aes(x = theoretical, y = sample)) +
          geom_point() +
          geom_abline(intercept = mean(dat_norm$y),
                      slope = sd(dat_norm$y),
                      linetype = "dashed", color = "red") +
          labs(x = "Theoretical Quantiles",
               y = "Sample Quantiles",
               title = "Normal Q-Q Plot",
               subtitle = "Points should follow the reference line",
               caption = paste0("Normality\n",
                                "Shapiro-Wilk Test: p = ",
                                signif(norm_test$p.value, 4))) +
          ggtheme

        # Combine plots
        plotpr <- wrap_plots(p2, p1, ncol = 2) &
         plot_annotation(
            theme = ggtheme
          )

        print(plotpr)

        return(TRUE)

      })
)
