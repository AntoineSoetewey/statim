#' @title T-Test: One-Sample and Two-Sample (`on`)
#'
#' @description
#' The `on` implementation performs a one-sample t-test for one or more
#' variables via [on()], or a two-sample t-test (independent or paired)
#' when exactly two variables are supplied and `via("two_sample")` is used.
#' The one-sample default tests each variable independently against a
#' hypothesized mean. The `two_sample` variant instead compares the two
#' variables to each other, without requiring the value/group layout
#' [x_by()] expects.
#'
#' @section Arguments:
#' The following arguments are passed via `...` in [T_TEST()] or [via()]:
#'
#' \describe{
#'   \item{`.mu`}{Numeric. Hypothesized mean (one-sample) or mean
#'     difference/contrast (`two_sample`). Default `0`.}
#'   \item{`.alt`}{Direction: `"two.sided"`, `"greater"`, or `"less"`. Default `"two.sided"`.}
#'   \item{`.ci`}{Confidence level. Default `0.95`.}
#'   \item{`.true_mu`}{One-sample only. Only meaningful via [state_null()]. Carries
#'     the scalar as written in the claim, purely for display in `true_mu`.
#'     Default `NULL`, falling back to `.mu`. Not intended to be set directly.}
#' }
#'
#' @section Variants:
#' \describe{
#'   \item{`"multi"`}{Performs independent one-sample t-tests across selected variables supplied via [on()].
#'     Accepts the same `.mu`, `.alt`, `.ci` arguments as the default. However, `.mu` is
#'     recycled across all variables or must match their count.}
#'   \item{`"two_sample"`}{Compares exactly two variables supplied via [on()].
#'     Accepts `.paired` (logical, default `FALSE`), `.var_equal` (logical,
#'     default `FALSE`, ignored when `.paired = TRUE`), and `.w` (a named
#'     numeric vector of contrast weights, one per variable, default `NULL`
#'     falling back to `c(1, -1)` in the order the variables were supplied).}
#' }
#'
#' @section One-sample t-test default class:
#' Applied on the default `ttest-on` and its variant `"multi"`. By default, returns a [class_ttest_one] object.
#' All variants that also return [class_ttest_one] inherit [auto_tidy()] and [print()] automatically. Otherwise,
#' to process outputs:
#'
#' -  `print()`: Write it down through `print` from [variant()].
#' -  `tidy()`: Use [making_tidy()] to register a tidy method if needed.
#'
#' @section Two-sample t-test class:
#' Only applied on `via("two_sample")`. By default, it returns a [class_ttest_two] object — the same class
#' produced by [ttest-xby]'s implementation. `group` holds a synthesized label
#' (e.g. `"1*x1 + -1*x2"`) rather than a grouping variable name, since `on()`
#' has no grouping column to name.
#'
#' @section Hypothesis claims:
#' Supports [MU()] via [state_null()]:
#'
#' ``` r
#' define_model(on(x), <data>) |>
#'     prepare_test(T_TEST) |>
#'     state_null(MU(x) >= 1) |>
#'     conclude()
#' ```
#'
#' Scaled claims are supported: `2 * MU(x) == 4` tests `MU(x) == 2`.
#' `true_mu` in the output shows the right-hand scalar as written (`4`), while
#' the test runs on the solved value (`2`).
#'
#' For `two_sample`, both variables from [on()] must appear in the claim, and
#' referenced by the same names given to (or auto-generated for) each
#' variable:
#'
#' ``` r
#' define_model(on(x1, x2), <data>) |>
#'     prepare_test(T_TEST) |>
#'     via("two_sample") |>
#'     state_null(MU(x1) - MU(x2) == 0) |>
#'     conclude()
#' ```
#'
#' Arbitrary linear contrasts are supported, including scaled terms and
#' constants on either side:
#'
#' ``` r
#' state_null(2 * MU(oj) + 1 == MU(vc) - 3)
#' ```
#'
#' `estimate` always reflects the sample contrast (`a * mean(x1) + b * mean(x2)`)
#' and does not change when only the hypothesized scalar changes, only
#' `t_stat`, `p_val`, and where the CI sits relative to the hypothesis shift
#' with it. This matches [stats::t.test()]'s own convention of reporting the
#' same `estimate` regardless of `mu`.
#'
#' A variable omitted from a `two_sample` claim, or a zero coefficient on
#' either variable, is an error rather than a silent one-sample reduction —
#' use `on(<single variable>)` with the default variant instead.
#'
#' @examples
#' # single variable
#' sleep |>
#'     define_model(on(extra)) |>
#'     prepare_test(T_TEST) |>
#'     conclude()
#'
#' # null hypothesis expression
#' sleep |>
#'     define_model(on(extra)) |>
#'     prepare_test(T_TEST) |>
#'     state_null(MU(extra) >= 1) |>
#'     conclude()
#'
#' # multiple variables
#' iris |>
#'     define_model(on(where(is.numeric))) |>
#'     prepare_test(T_TEST) |>
#'     via("multi") |>
#'     conclude()
#'
#' # two-sample, wide-format columns, unpaired (Welch by default)
#' vc = ToothGrowth$len[ToothGrowth$supp == "VC"]
#' oj = ToothGrowth$len[ToothGrowth$supp == "OJ"]
#'
#' define_model(on(vc, oj)) |>
#'     prepare_test(T_TEST) |>
#'     via("two_sample") |>
#'     conclude()
#'
#' # two-sample, paired
#' # You can use the `I()` and `with()` call
#' # To refer the columns as a local environment
#' # Containing the data
#' ToothGrowth |>
#'     with(define_model(on(
#'         I(d1 = len[supp == "OJ" & dose == 1]),
#'         I(d2 = len[supp == "VC" & dose == 1])
#'     ))) |>
#'     prepare_test(T_TEST) |>
#'     via("two_sample", .paired = TRUE) |>
#'     conclude()
#'
#' # two-sample with a weighted contrast hypothesis
#' ToothGrowth |>
#'     with(define_model(on(I(oj = len[supp == "OJ"]), I(vc = len[supp == "VC"])))) |>
#'     prepare_test(T_TEST) |>
#'     via("two_sample") |>
#'     state_null(2 * MU(oj) - MU(vc) == 5) |>
#'     conclude()
#'
#' @seealso [ttest-xby] for the value/group layout, [class_ttest_two],
#'     [state_null()]
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

                if (length(data) != 1L) {
                    cli::cli_abort(c(
                        "One-sample t-test (base) requires exactly 1 variable.",
                        "i" = "Found {length(data)} variable{cli::qty(length(data))}{?s}.",
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
            n_vars = length(data)

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
                    df = unname(res$parameter),
                    p_val = res$p.value,
                    lower_ci = res$conf.int[[1]],
                    upper_ci = res$conf.int[[2]]
                )
            })

            class_ttest_one(
                term = vapply(tests, \(t) t$term, character(1)),
                estimate = vapply(tests, \(t) t$estimate, numeric(1)),
                true_mu = .mu,
                df = vapply(tests, \(t) t$df, numeric(1)),
                t_stat = vapply(tests, \(t) t$statistic, numeric(1)),
                p_val = vapply(tests, \(t) t$p_val, numeric(1)),
                lower_ci = vapply(tests, \(t) t$lower_ci, numeric(1)),
                upper_ci = vapply(tests, \(t) t$upper_ci, numeric(1)),
                ci_level = .ci
            )
        }),
        two_sample = variant(
            fn = function(
                .proc,
                .mu = 0,
                .paired = FALSE,
                .var_equal = FALSE,
                .alt = "two.sided",
                .ci = 0.95,
                .w = NULL
            ) {
                data = .proc$data

                if (length(data) != 2L) {
                    cli::cli_abort(c(
                        "Two-sample t-test (on) requires exactly 2 variables.",
                        "i" = "Found {length(data)} variable{cli::qty(length(data))}{?s}.",
                        "i" = "Use {.fn x_by} for a value/group layout instead."
                    ))
                }

                term1 = names(data)[[1]]
                term2 = names(data)[[2]]
                x1 = data[[1]]
                x2 = data[[2]]

                w = resolve_two_sample_weights(.w, term1, term2)
                a = w[[term1]]
                b = w[[term2]]
                label = paste0(a, "*", term1, " + ", b, "*", term2)

                if (.paired) {
                    if (length(x1) != length(x2)) {
                        cli::cli_abort(c(
                            "Paired t-test requires both variables to be the same length.",
                            "x" = "{.arg {term1}} has length {length(x1)}, {.arg {term2}} has length {length(x2)}."
                        ))
                    }

                    combined = a * x1 + b * x2
                    res = stats::t.test(
                        x = combined,
                        mu = .mu,
                        alternative = .alt,
                        conf.level = .ci
                    )

                    return(class_ttest_two(
                        group = label,
                        estimate = unname(res$estimate),
                        t_stat = unname(res$statistic),
                        df = unname(res$parameter),
                        p_val = res$p.value,
                        lower_ci = res$conf.int[[1]],
                        upper_ci = res$conf.int[[2]],
                        ci_level = .ci
                    ))
                }

                n1 = length(x1)
                n2 = length(x2)
                xbar1 = mean(x1)
                xbar2 = mean(x2)
                s1 = stats::var(x1)
                s2 = stats::var(x2)
                est_val = a * xbar1 + b * xbar2

                if (.var_equal) {
                    df = n1 + n2 - 2
                    pooled_var = ((n1 - 1) * s1 + (n2 - 1) * s2) / df
                    se = sqrt(pooled_var * (a^2 / n1 + b^2 / n2))
                } else {
                    se = sqrt(a^2 * s1 / n1 + b^2 * s2 / n2)
                    df = se^4 /
                        ((a^2 * s1 / n1)^2 /
                            (n1 - 1) +
                            (b^2 * s2 / n2)^2 / (n2 - 1))
                }

                tstat = (est_val - .mu) / se

                p_val = switch(
                    .alt,
                    "two.sided" = 2 * stats::pt(-abs(tstat), df = df),
                    "greater" = stats::pt(tstat, df = df, lower.tail = FALSE),
                    "less" = stats::pt(tstat, df = df)
                )

                alpha = 1 - .ci
                ci = switch(
                    .alt,
                    "two.sided" = {
                        t_crit = stats::qt(1 - alpha / 2, df = df)
                        c(est_val - t_crit * se, est_val + t_crit * se)
                    },
                    "greater" = {
                        t_crit = stats::qt(1 - alpha, df = df)
                        c(est_val - t_crit * se, Inf)
                    },
                    "less" = {
                        t_crit = stats::qt(1 - alpha, df = df)
                        c(-Inf, est_val + t_crit * se)
                    }
                )

                class_ttest_two(
                    group = label,
                    estimate = est_val,
                    t_stat = tstat,
                    df = df,
                    p_val = p_val,
                    lower_ci = ci[[1]],
                    upper_ci = ci[[2]],
                    ci_level = .ci
                )
            },
            claim_parser = map_claim(
                .mu = function(claim, processed) {
                    claim_contrast_coefs(claim)$scalar
                },
                .alt = function(claim, processed) {
                    switch(
                        claim_contrast_coefs(claim)$op,
                        "==" = ,
                        "!=" = "two.sided",
                        ">=" = ,
                        ">" = "less",
                        "<=" = ,
                        "<" = "greater"
                    )
                },
                .w = function(claim, processed) {
                    claim_contrast_coefs(claim)$coefs
                }
            )
        )
    ),
    compatible_params = list(MU)
)

resolve_two_sample_weights = function(w, term1, term2) {
    if (is.null(w)) {
        w = c(1, -1)
        names(w) = c(term1, term2)
        return(w)
    }

    nms = names(w)
    if (is.null(nms) || any(!nzchar(nms))) {
        cli::cli_abort(c(
            "Contrast weights must be named to match {.fn on}'s variables.",
            "i" = "Expected names {.val {c(term1, term2)}}."
        ))
    }

    missing_terms = setdiff(c(term1, term2), nms)
    if (length(missing_terms) > 0L) {
        cli::cli_abort(c(
            "Hypothesis omits a variable supplied to {.fn on}.",
            "x" = "No coefficient found for {.val {missing_terms}}.",
            "i" = "Variables in {.fn on} are {.val {c(term1, term2)}}."
        ))
    }

    extra_terms = setdiff(nms, c(term1, term2))
    if (length(extra_terms) > 0L) {
        cli::cli_abort(c(
            "Hypothesis references variable{cli::qty(length(extra_terms))}{?s} not in {.fn on}: {.val {extra_terms}}.",
            "i" = "Variables in {.fn on} are {.val {c(term1, term2)}}."
        ))
    }

    w = w[c(term1, term2)]
    if (any(w == 0)) {
        cli::cli_abort(c(
            "Both coefficients on {.fn on}'s variables must be non-zero.",
            "i" = "A zero coefficient reduces this to a one-sample test.",
            "i" = "Use {.code on({term1})} or {.code on({term2})} alone instead."
        ))
    }

    w
}
