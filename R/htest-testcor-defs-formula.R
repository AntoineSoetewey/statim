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
#' The following arguments are passed via `...` in [CORTEST()]:
#'
#' \describe{
#'   \item{`.cor_type`}{String. One of `"pearson"`, `"spearman"`, or
#'     `"kendall"`. Default `"pearson"`.}
#'   \item{`.alt`}{String. One of `"two.sided"`, `"greater"`, or `"less"`.
#'     Default `"two.sided"`.}
#'   \item{`.ci`}{Numeric. Confidence level. Default `0.95`. Only used for
#'     Pearson; silently ignored for Kendall and Spearman.}
#'   \item{`.rho`}{Numeric. Hypothesized population correlation. Default `0`.
#'     Only used for Pearson; triggers Fisher-z test when non-zero.}
#' }
#'
#' @section Correlation test default class:
#' Returns a [class_corr_two] object inheriting from [class_stat_infer].
#'
#' @examples
#' cars |>
#'     define_model(dist ~ speed) |>
#'     prepare_test(CORTEST) |>
#'     conclude()
#'
#' # multiple independent variables
#' mtcars |>
#'     define_model(mpg ~ wt + hp) |>
#'     prepare_test(CORTEST) |>
#'     conclude()
#'
#' @keywords internal
#' @name cortest-formula
#' @family cortest-implementations
NULL

cor_test_formula = test_define(
    model_type = S7::class_formula,
    impl = agendas(
        base = baseline(
            fn = function(.proc, .cor_type = "pearson", .alt = "two.sided", .ci = 0.95, .rho = 0) {
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

                if (.rho != 0 && .cor_type != "pearson") {
                    cli::cli_abort(c(
                        "Non-zero {.arg .rho} is only supported for Pearson correlation.",
                        "i" = "Got {.arg .cor_type} = {.val {.cor_type}}.",
                        "i" = "Either set {.code .cor_type = \"pearson\"} or use {.code .rho = 0}."
                    ))
                }

                if (.rho != 0) {
                    results = lapply(rhs_labels, function(x_name) {
                        pearson_fisher_z(
                            x = data[[x_name]],
                            y = data[[resp_name]],
                            ind_vars = x_name,
                            resp_vars = resp_name,
                            .rho = .rho,
                            .alt = .alt,
                            .ci = .ci
                        )
                    })

                    return(combine_corr_two(results, .ci))
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
                    estimate = vapply(tests, function(t) unname(t$res$estimate), numeric(1)),
                    statistic = vapply(tests, function(t) unname(t$res$statistic), numeric(1)),
                    df = if (has_df) {
                        vapply(tests, function(t) unname(t$res$parameter), numeric(1))
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
            }
        )
    )
)

combine_corr_two = function(results, .ci) {
    class_corr_two(
        ind_vars = vapply(results, function(r) r@ind_vars, character(1)),
        resp_vars = vapply(results, function(r) r@resp_vars, character(1)),
        estimate = vapply(results, function(r) r@estimate, numeric(1)),
        statistic = vapply(results, function(r) r@statistic, numeric(1)),
        df = numeric(0),
        p_val = vapply(results, function(r) r@p_val, numeric(1)),
        lower_ci = vapply(results, function(r) r@lower_ci, numeric(1)),
        upper_ci = vapply(results, function(r) r@upper_ci, numeric(1)),
        ci_level = .ci
    )
}
