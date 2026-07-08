#' @title T-Test: Two-Sample (`x_by`)
#'
#' @description
#' The `x_by` implementation performs an independent or paired two-sample
#' t-test. It accepts one or more grouping variables via [x_by()].
#'
#' @section Arguments:
#' The following arguments are passed via `...` in [T_TEST()] or [via()]:
#'
#' \describe{
#'   \item{`.paired`}{Logical. Whether to perform a paired t-test. Default `FALSE`.}
#'   \item{`.mu`}{Numeric. Hypothesized mean difference. Default `0`.}
#'   \item{`.alt`}{Direction: `"two.sided"`, `"greater"`, or `"less"`. Default `"two.sided"`.}
#'   \item{`.ci`}{Confidence level. Default `0.95`.}
#'   \item{`.first_group`}{Only if uses [state_null()]. Considers first term as the first order. Default is `NULL`.}
#' }
#'
#' @section Variants:
#' \describe{
#'   \item{`"boot"`}{Bootstrap CI. Accepts `n` (reps) and `seed`.}
#'   \item{`"permute"`}{Permutation test. Accepts `n` and `seed`.}
#'   \item{`"contrast"`}{Welch-Satterthwaite linear contrast test. Accepts `.w`, `.mu`, `.ci`, `.op`.}
#'   \item{`"multi"`}{Accepts multiple selected `group` variables}
#' }
#'
#' @section Two-sample t-test default class:
#' By default, returns a [class_ttest_two] object. All variants that also return
#' [class_ttest_two] inherit [auto_tidy()] and [print()] automatically. Otherwise,
#' to process outputs:
#'
#' -  `print()`: Write it down through `print` from [variant()].
#' -  `tidy()`: Use [making_tidy()] to register a tidy method if needed.
#'
#' @section Hypothesis claims:
#' Supports [MU()] via [state_null()]. The `contrast` variant performs Welch-Satterthwaite linear contrast test
#' and additionally accepts contrast coefficients via `.w`.
#'
#' Claim order is respected: writing `MU(x, g == "a") - MU(x, g == "b")` versus
#' `MU(x, g == "b") - MU(x, g == "a")` flips the sign of `estimate` and
#' `t_stat`, since the group with coefficient `+1` in the parsed claim becomes
#' `x` in `stats::t.test()`. This is implemented via an internal `.first_group`
#' argument resolved from the claim — it is not meant to be set directly by
#' users. If you call `via("base", .first_group = ...)` or use `update()` to
#' override it manually, note that it accepts a single group label (one of
#' the two levels of the grouping variable) and silently falls back to the
#' data's natural level order (`unique()` on the grouping variable) if `NULL`,
#' unset, or not found among the levels.
#'
#' @examples
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(T_TEST) |>
#'     conclude()
#'
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(T_TEST) |>
#'     via("boot", n = 2000) |>
#'     conclude()
#'
#' # contrast t-test, which allows `state_null()` to have weights
#' # Around population parameter function `MU()` notation
#' # Also `%by%` is just the infixed form of `x_by()`
#' sleep |>
#'     define_model(extra %by% group) |>
#'     prepare_test(T_TEST) |>
#'     state_null(
#'         2 * MU(extra, group == "1") - MU(extra, group == "2") <= 0
#'     ) |>
#'     via("contrast") |>
#'     conclude()
#'
#' @section References:
#' Welch, B. L. (1947). The generalization of "Student's" problem when
#' several different population variances are involved. *Biometrika*,
#' 34(1-2), 28-35. \url{https://doi.org/10.1093/biomet/34.1-2.28}
#'
#' Satterthwaite, F. E. (1946). An approximate distribution of estimates
#' of variance components. *Biometrics Bulletin*, 2(6), 110-114.
#' \url{https://doi.org/10.2307/3002019}
#'
#' Kutner, M. H., Nachtsheim, C. J., Neter, J., & Li, W. (2004).
#' *Applied Linear Statistical Models* (5th ed.). McGraw-Hill/Irwin.
#'
#' @keywords internal
#' @name ttest-xby
#' @family ttest-implementations
NULL

ttest_def_two = test_define(
    model_type = x_by,
    impl = agendas(
        base = baseline(
            # ---- Default implementation (single grouping variable) ----
            fn = function(
                .proc,
                .paired = FALSE,
                .mu = 0,
                .alt = "two.sided",
                .ci = 0.95,
                .first_group = NULL
            ) {
                x = .proc$x_data[[1]]
                group_data = .proc$group_data

                if (length(group_data) != 1L) {
                    cli::cli_abort(c(
                        "Two-sample t-test requires exactly 1 grouping variable.",
                        "i" = "Found {length(group_data)} grouping variable{cli::qty(length(group_data))}{?s}.",
                        "i" = "Use {.code via(\"multi\")} (e.g. {.code ... |> prepare(T_TEST) |> via(\"multi\")}) to test multiple grouping variables."
                    ))
                }

                grp_name = names(group_data)[[1]]
                grp = as.character(group_data[[grp_name]])
                lvls = unique(grp)

                if (length(lvls) != 2L) {
                    cli::cli_abort(c(
                        "Two-sample t-test requires exactly 2 groups.",
                        "i" = "Found {length(lvls)} group{{?s}} in {.val {grp_name}}."
                    ))
                }

                if (!is.null(.first_group) && length(.first_group) == 1L) {
                    if (!.first_group %in% lvls) {
                        cli::cli_abort(c(
                            "Hypothesis references group {.val {.first_group}},",
                            "i" = "but {.val {grp_name}} only has levels {.val {lvls}}."
                        ))
                    }
                    if (lvls[[1]] != .first_group) lvls = rev(lvls)
                }

                res = stats::t.test(
                    x = x[grp == lvls[[1]]],
                    y = x[grp == lvls[[2]]],
                    paired = .paired,
                    mu = .mu,
                    alternative = .alt,
                    conf.level = .ci
                )

                estimate = if (.paired) {
                    unname(res$estimate)
                } else {
                    unname(res$estimate[[1]] - res$estimate[[2]])
                }

                class_ttest_two(
                    group = grp_name,
                    estimate = estimate,
                    t_stat = unname(res$statistic),
                    df = unname(res$parameter),
                    p_val = res$p.value,
                    lower_ci = res$conf.int[[1]],
                    upper_ci = res$conf.int[[2]],
                    ci_level = .ci
                )
            },
            claim_parser = map_claim(
                .mu = function(claim, processed) {
                    resolved = claim_contrast_coefs(claim, filter = "given")
                    coefs = resolved$coefs

                    valid_two_sample = length(coefs) == 2L &&
                        identical(sort(unname(coefs)), c(-1, 1))

                    if (!valid_two_sample) {
                        cli::cli_abort(c(
                            "T-test for `x_by()` only supports two-sample mean differences.",
                            "i" = "Found contrast coefficients: {.val {coefs}}.",
                            "i" = "Use {.code via(\"contrast\")} for weighted/contrast hypotheses,",
                            "i" = "or use a formula model for one-sample tests."
                        ))
                    }

                    resolved$scalar
                },
                .first_group = function(claim, processed) {
                    resolved = claim_contrast_coefs(claim, filter = "given")
                    names(resolved$coefs)[resolved$coefs == 1]
                },
                .alt = function(claim, processed = NULL) {
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
        contrast = variant(
            # ---- contrast t-test ----
            # ---- variant: "contrast" ----
            fn = function(.proc, .mu = 0, .ci = 0.95, .w = NULL, .op = "==") {
                x = .proc$x_data[[1]]
                group_data = .proc$group_data

                grp_name = names(group_data)[[1]]
                grp = as.character(group_data[[grp_name]])
                lvls = unique(grp)

                if (length(lvls) != 2L) {
                    cli::cli_abort(c(
                        "Contrast t-test requires exactly 2 groups.",
                        "i" = "Found {length(lvls)} group{{?s}} in {.val {grp_name}}."
                    ))
                }

                x1 = x[grp == lvls[[1]]]
                x2 = x[grp == lvls[[2]]]
                n1 = length(x1)
                n2 = length(x2)
                xbar1 = mean(x1)
                xbar2 = mean(x2)
                s1 = stats::var(x1)
                s2 = stats::var(x2)

                coefs = if (is.null(.w)) {
                    c(1, -1)
                } else {
                    coef_nms = names(.w)
                    c(.w[coef_nms == lvls[[1]]], .w[coef_nms == lvls[[2]]])
                }

                c1 = coefs[[1]]
                c2 = coefs[[2]]
                est_val = c1 * xbar1 + c2 * xbar2
                se = sqrt(c1^2 * s1 / n1 + c2^2 * s2 / n2)
                tstat = (est_val - .mu) / se
                df = (c1^2 * s1 / n1 + c2^2 * s2 / n2)^2 /
                    ((c1^2 * s1 / n1)^2 /
                        (n1 - 1) +
                        (c2^2 * s2 / n2)^2 / (n2 - 1))

                p.value = switch(
                    .op,
                    "==" = 2 * stats::pt(-abs(tstat), df = df),
                    ">=" = ,
                    ">" = stats::pt(-tstat, df = df, lower.tail = FALSE),
                    "<=" = ,
                    "<" = stats::pt(-tstat, df = df),
                    "!=" = 2 * stats::pt(-abs(tstat), df = df)
                )

                alpha = 1 - .ci
                ci = switch(
                    .op,
                    "==" = ,
                    "!=" = {
                        t_crit = stats::qt(1 - alpha / 2, df = df)
                        c(est_val - t_crit * se, est_val + t_crit * se)
                    },
                    "<=" = ,
                    "<" = {
                        t_crit = stats::qt(1 - alpha, df = df)
                        c(est_val - t_crit * se, Inf)
                    },
                    ">=" = ,
                    ">" = {
                        t_crit = stats::qt(1 - alpha, df = df)
                        c(-Inf, est_val + t_crit * se)
                    }
                )

                class_ttest_two(
                    group = grp_name,
                    estimate = est_val,
                    t_stat = tstat,
                    df = df,
                    p_val = p.value,
                    lower_ci = ci[[1]],
                    upper_ci = ci[[2]],
                    ci_level = .ci
                )
            },
            claim_parser = map_claim(
                .mu = function(claim, processed) {
                    claim_contrast_coefs(claim)$scalar
                },
                .op = function(claim, processed) claim_contrast_coefs(claim)$op,
                .w = function(claim, processed) {
                    claim_contrast_coefs(claim)$coefs
                }
            )
        ),
        multi = variant(
            # ---- Multiple grouping variables ----
            # ---- variant: multi ----
            fn = function(
                .proc,
                .paired = FALSE,
                .mu = 0,
                .alt = "two.sided",
                .ci = 0.95
            ) {
                x = .proc$x_data[[1]]
                group_data = .proc$group_data
                n_groups = length(group_data)

                if (length(.mu) == 1L) {
                    .mu = rep(.mu, n_groups)
                } else if (length(.mu) != n_groups) {
                    cli::cli_abort(c(
                        "`.mu` must be length 1 or match the number of grouping variables.",
                        "i" = "Found {length(group_data)} grouping variable{cli::qty(n_groups)}{?s},",
                        "i" = "but {.arg .mu} has length {length(.mu)}."
                    ))
                }

                tests = lapply(seq_along(group_data), function(i) {
                    grp_name = names(group_data)[[i]]
                    grp = as.character(group_data[[grp_name]])
                    lvls = unique(grp)

                    if (length(lvls) != 2L) {
                        cli::cli_abort(c(
                            "Two-sample t-test requires exactly 2 groups.",
                            "i" = "Found {length(lvls)} group{{?s}} in {.val {grp_name}}."
                        ))
                    }

                    res = stats::t.test(
                        x = x[grp == lvls[[1]]],
                        y = x[grp == lvls[[2]]],
                        paired = .paired,
                        mu = .mu[[i]],
                        alternative = .alt,
                        conf.level = .ci
                    )

                    list(
                        group = grp_name,
                        estimate = if (.paired) {
                            unname(res$estimate)
                        } else {
                            unname(res$estimate[[1]] - res$estimate[[2]])
                        },
                        t_stat = unname(res$statistic),
                        df = unname(res$parameter),
                        p_val = res$p.value,
                        lower_ci = res$conf.int[[1]],
                        upper_ci = res$conf.int[[2]]
                    )
                })

                class_ttest_two(
                    group = vapply(tests, \(x) x$group, character(1)),
                    estimate = vapply(tests, \(x) x$estimate, numeric(1)),
                    t_stat = vapply(tests, \(x) x$t_stat, numeric(1)),
                    df = vapply(tests, \(x) x$df, numeric(1)),
                    p_val = vapply(tests, \(x) x$p_val, numeric(1)),
                    lower_ci = vapply(tests, \(x) x$lower_ci, numeric(1)),
                    upper_ci = vapply(tests, \(x) x$upper_ci, numeric(1)),
                    ci_level = .ci
                )
            }
        ),
        boot = variant(
            # ---- Bootstrapping ----
            # ---- variant: boot ----
            fn = function(.proc, .ci = 0.95, n = 1000L, seed = NULL) {
                x = .proc$x_data[[1]]
                group_data = .proc$group_data

                if (!is.null(seed)) {
                    set.seed(seed)
                }

                grp = as.character(group_data[[1]])
                lvls = unique(grp)

                idx1 = which(grp == lvls[[1]])
                idx2 = which(grp == lvls[[2]])

                boot_dist = replicate(n, {
                    b1 = x[sample(idx1, replace = TRUE)]
                    b2 = x[sample(idx2, replace = TRUE)]
                    mean(b1) - mean(b2)
                })

                ci = quantile(boot_dist, c((1 - .ci) / 2, 1 - (1 - .ci) / 2))
                list(boot_dist = boot_dist, ci = ci, n = n)
            },
            print = function(x, ...) {
                ci = round(x@data$ci, 4)
                summary_data = tibble::tibble(
                    names = c("CI", "n_reps"),
                    vals = c(paste0("[", ci[[1]], ", ", ci[[2]], "]"), x@data$n)
                )

                cli::cat_line(
                    cli::rule(center = "Bootstrapped T-test", line = "="),
                    "\n\n"
                )
                cli::cat_line(cli::rule(left = "Summary", line = "-"), "\n")
                tabstats::table_summary(
                    summary_data,
                    style = tabstats::sm_style(sep = ":  "),
                    center_table = TRUE
                )
                cat("\n\n")
                invisible(x)
            }
        ),
        permute = variant(
            # ---- Permutation test ----
            # ---- variant: permute ----
            fn = function(.proc, n = 1000L, seed = NULL) {
                x = .proc$x_data[[1]]
                group_data = .proc$group_data

                if (!is.null(seed)) {
                    set.seed(seed)
                }

                grp = as.character(group_data[[1]])
                lvls = unique(grp)

                obs = mean(x[grp == lvls[[1]]]) - mean(x[grp == lvls[[2]]])

                null_dist = replicate(n, {
                    perm = sample(x)
                    mean(perm[grp == lvls[[1]]]) - mean(perm[grp == lvls[[2]]])
                })

                list(
                    observed = obs,
                    null_dist = null_dist,
                    p.value = mean(abs(null_dist) >= abs(obs)),
                    n = n
                )
            },
            print = function(x, ...) {
                summary_data = tibble::tibble(
                    Statistic = round(x@data$observed, 4),
                    `p-value` = round(x@data$p.value, 4),
                    n_perms = x@data$n
                )

                pval_styler = function(x) {
                    x_num = suppressWarnings(as.numeric(x$value))
                    if (is.na(x_num) || x_num > 0.05) {
                        cli::style_italic(x$value)
                    } else if (x_num > 0.01) {
                        cli::col_red(x$value)
                    } else {
                        cli::style_bold("<0.001")
                    }
                }

                cli::cat_line(
                    cli::rule(center = "T-test Permutation", line = "="),
                    "\n\n"
                )
                cli::cat_line(cli::rule(left = "Summary", line = "-"), "\n")
                tabstats::table_default(
                    summary_data,
                    style_columns = tabstats::td_style(`p-value` = pval_styler)
                )
                cat("\n\n")
                invisible(x)
            }
        )
    ),
    # ---- Modify Modelled Hypothesis ----
    compatible_params = list(MU)
)
