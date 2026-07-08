#' @title Correlation Test: Formula interface
#'
#' @description
#' The formula implementation performs pairwise correlation tests between a
#' single response variable (LHS) and one or more independent variables (RHS).
#'
#' 1. `y ~ x`: one independent variable, one correlation test.
#' 2. `y ~ x1 + x2`: multiple independent variables, one test per RHS term.
#'
#' Use a formula directly as the model ID to select this implementation.
#'
#' @section Arguments:
#' The following arguments are passed via `...` in [COR_TEST()]:
#'
#' \describe{
#'   \item{`.cor_type`}{String. One of `"pearson"`, `"spearman"`, or
#'     `"kendall"`. Default `"pearson"`.}
#'   \item{`.alt`}{String. One of `"two.sided"`, `"greater"`, or `"less"`.
#'     Default `"two.sided"`.}
#'   \item{`.ci`}{Numeric. Confidence level. Default `0.95`. Only used for
#'     Pearson; silently ignored for Kendall and Spearman.}
#' }
#'
#' @section Correlation test default class:
#' As detailed by [cortest-rel], it returns a [class_corr_two] object inheriting
#' from [class_stat_infer] by default. You need to process outputs by:
#'
#' -  `print()`: Write it down through `print` from [variant()].
#' -  `tidy()`: Use [making_tidy()] to register a tidy method if needed.
#'
#' if the variants from this method pipeline doesn't return a [class_corr_two] object.
#'
#' @section Hypothesis claims:
#' Not supported. Use [rel()] with the `base` variant for [state_null()]
#' with [RHO()].
#'
#' @examples
#' cars |>
#'     define_model(dist ~ speed) |>
#'     prepare_test(COR_TEST) |>
#'     conclude()
#'
#' # multiple independent variables
#' mtcars |>
#'     define_model(mpg ~ wt + hp) |>
#'     prepare_test(COR_TEST) |>
#'     conclude()
#'
#' @keywords internal
#' @name cortest-formula
#' @family cortest-implementations
NULL

cor_test_formula = test_define(
    model_type = S7::class_formula,
    impl = agendas(
        base = baseline(fn = function(
            .proc,
            .cor_type = "pearson",
            .alt = "two.sided",
            .ci = 0.95
        ) {
            formula = .proc$formula
            data = .proc$data

            resp_name = all.vars(formula)[1]
            rhs_labels = attr(terms(formula), "term.labels")

            if (length(rhs_labels) == 0L) {
                cli::cli_abort(c(
                    "Formula must have at least one RHS term.",
                    "i" = "Use {.code y ~ x} or {.code y ~ x1 + x2}."
                ))
            }

            if (nrow(data) < 4L) {
                cli::cli_abort(c(
                    "Correlation test requires at least 4 observations.",
                    "i" = "Got {nrow(data)}."
                ))
            }

            tests = lapply(rhs_labels, function(x_name) {
                res = stats::cor.test(
                    x = data[[x_name]],
                    y = data[[resp_name]],
                    method = .cor_type,
                    alternative = .alt,
                    conf.level = .ci
                )
                list(x_name = x_name, res = res)
            })

            has_ci = !is.null(tests[[1]]$res$conf.int)
            has_df = !is.null(tests[[1]]$res$parameter)

            class_corr_two(
                ind_vars = vapply(tests, `[[`, character(1), "x_name"),
                resp_vars = rep(resp_name, length(tests)),
                estimate = vapply(
                    tests,
                    function(t) unname(t$res$estimate),
                    numeric(1)
                ),
                statistic = vapply(
                    tests,
                    function(t) unname(t$res$statistic),
                    numeric(1)
                ),
                df = if (has_df) {
                    vapply(
                        tests,
                        function(t) unname(t$res$parameter),
                        numeric(1)
                    )
                } else {
                    numeric(0)
                },
                p_val = vapply(tests, function(t) t$res$p.value, numeric(1)),
                lower_ci = if (has_ci) {
                    vapply(tests, function(t) t$res$conf.int[[1]], numeric(1))
                } else {
                    numeric(0)
                },
                upper_ci = if (has_ci) {
                    vapply(tests, function(t) t$res$conf.int[[2]], numeric(1))
                } else {
                    numeric(0)
                },
                ci_level = .ci
            )
        })
    )
)
