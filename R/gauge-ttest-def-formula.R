making_gauge(T_TEST, S7::class_formula) %<-%
    method_gauge(
        default = function(object, quiet = TRUE, ...) {
            dat = object@data

            if (!quiet) {
                cli::cli_inform(c(
                    "Cohen's d for formula-based t-tests is approximated as {.code 2 * t / sqrt(df)}.",
                    "i" = "This assumes roughly equal group sizes and may be inaccurate otherwise.",
                    "i" = "Pass {.code quiet = TRUE} to suppress this message."
                ))
            }

            d = vapply(
                dat$ttest,
                function(h) {
                    2 * unname(h$statistic) / sqrt(unname(h$parameter))
                },
                numeric(1)
            )

            tibble::tibble(
                type = dat$type,
                group = dat$group,
                metric = "cohens_d_approx",
                value = d
            )
        }
    )
