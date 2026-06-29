#' @title T-Test: One-Sample (`on`)
#'
#' @description
#' The `on` implementation performs a one-sample t-test for one or more
#' variables via [on()]. Each variable is tested independently against a
#' hypothesized mean.
#'
#' @section Arguments:
#' The following arguments are passed via `...` in [TTEST()] or [via()]:
#'
#' \describe{
#'   \item{`.mu`}{Numeric. Hypothesized mean under H\eqn{_0}. Default `0`.}
#'   \item{`.alt`}{Direction: `"two.sided"`, `"greater"`, or `"less"`. Default `"two.sided"`.}
#'   \item{`.ci`}{Confidence level. Default `0.95`.}
#'   \item{`.true_mu`}{Only meaningful via [state_null()]. Carries the scalar as
#'     written in the claim, purely for display in `true_mu`. Default `NULL`,
#'     falling back to `.mu`. Not intended to be set directly.}
#' }
#'
#' @section Variants:
#' \describe{
#'   \item{`"multi"`}{Tests multiple variables supplied via [on()]. Accepts the
#'     same `.mu`, `.alt`, `.ci` arguments as the default. `.mu` is recycled
#'     across all variables or must match their count.}
#' }
#'
#' @section Hypothesis claims:
#' Supports [MU()] via [state_null()]:
#'
#' ```r
#' define_model(on(extra), sleep) |>
#'     prepare_test(TTEST) |>
#'     state_null(MU(extra) >= 1) |>
#'     conclude()
#' ```
#'
#' Scaled claims are supported: `2 * MU(extra) == 4` tests `MU(extra) == 2`.
#' `true_mu` in the output shows the right-hand scalar as written (`4`), while
#' the test runs on the solved value (`2`).
#'
#' @examples
#' # single variable
#' sleep |>
#'     define_model(on(extra)) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' # hypothesis claim
#' sleep |>
#'     define_model(on(extra)) |>
#'     prepare_test(TTEST) |>
#'     state_null(MU(extra) >= 1) |>
#'     conclude()
#'
#' # multiple variables
#' sleep |>
#'     define_model(on(extra)) |>
#'     prepare_test(TTEST) |>
#'     via("multi") |>
#'     conclude()
#'
#' @keywords internal
#' @name ttest-on
#' @family ttest-implementations
NULL

ttest_def_on = test_define(
    model_type = on,
    impl = agendas(
        base = baseline(
            fn = function(
                .proc,
                .mu = 0,
                .alt = "two.sided",
                .ci = 0.95,
                .true_mu = NULL
            ) {
                data = .proc$data

                if (ncol(data) != 1L) {
                    cli::cli_abort(c(
                        "One-sample t-test (base) requires exactly 1 variable.",
                        "i" = "Found {ncol(data)} variable{cli::qty(ncol(data))}{?s}.",
                        "i" = "Use {.code via(\"multi\")} to test multiple variables."
                    ))
                }

                term = names(data)[[1]]
                x = data[[1]]

                res = stats::t.test(
                    x = x,
                    mu = .mu,
                    alternative = .alt,
                    conf.level = .ci
                )

                class_ttest_one(
                    term = term,
                    estimate = unname(res$estimate),
                    true_mu = .true_mu %||% .mu,
                    df = unname(res$parameter),
                    t_stat = unname(res$statistic),
                    p_val = res$p.value,
                    lower_ci = res$conf.int[[1]],
                    upper_ci = res$conf.int[[2]],
                    ci_level = .ci
                )
            },
            claim_parser = map_claim(
                .mu = function(claim, processed) {
                    claim_scalar(claim, solve_coef = TRUE)$scalar
                },
                .true_mu = function(claim, processed) {
                    claim_scalar(claim, solve_coef = FALSE)$scalar
                },
                .alt = function(claim, processed) {
                    switch(
                        claim@op,
                        "==" = ,
                        "!=" = "two.sided",
                        ">=" = ,
                        ">" = "less",
                        "<=" = ,
                        "<" = "greater"
                    )
                }
            )
        ),
        multi = variant(fn = function(
            .proc,
            .mu = 0,
            .alt = "two.sided",
            .ci = 0.95
        ) {
            data = .proc$data
            n_vars = ncol(data)

            if (length(.mu) == 1L) {
                .mu = rep(.mu, n_vars)
            } else if (length(.mu) != n_vars) {
                cli::cli_abort(c(
                    "`.mu` must be length 1 or match the number of variables.",
                    "i" = "Found {n_vars} variable{cli::qty(n_vars)}{?s},",
                    "i" = "but {.arg .mu} has length {length(.mu)}."
                ))
            }

            tests = lapply(seq_len(n_vars), function(i) {
                term = names(data)[[i]]
                x = data[[i]]

                res = stats::t.test(
                    x = x,
                    mu = .mu[[i]],
                    alternative = .alt,
                    conf.level = .ci
                )

                list(
                    term = term,
                    estimate = unname(res$estimate),
                    statistic = unname(res$statistic),
                    p_val = res$p.value,
                    lower_ci = res$conf.int[[1]],
                    upper_ci = res$conf.int[[2]]
                )
            })

            class_ttest_one(
                term = vapply(tests, \(t) t$term, character(1)),
                estimate = vapply(tests, \(t) t$estimate, numeric(1)),
                true_mu = .mu,
                df = vapply(tests, \(t) t$parameter, numeric(1)),
                t_stat = vapply(tests, \(t) t$statistic, numeric(1)),
                p_val = vapply(tests, \(t) t$p_val, numeric(1)),
                lower_ci = vapply(tests, \(t) t$lower_ci, numeric(1)),
                upper_ci = vapply(tests, \(t) t$upper_ci, numeric(1)),
                ci_level = .ci
            )
        })
    ),
    compatible_params = list(MU)
)
