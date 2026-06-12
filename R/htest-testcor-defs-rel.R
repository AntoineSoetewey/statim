main_cortest_rel = function(.cor_type) {
    function(.proc, .alt = "two.sided", .ci = 0.95) {
        x_data = .proc$x_data
        resp_data = .proc$resp_data

        if (length(x_data) != 1L) {
            cli::cli_abort(c(
                "{.arg x} must be a single variable for {.fn rel}.",
                "i" = "Got {length(x_data)} variable{?s}: {.val {names(x_data)}}.",
                "i" = "Use a bare name or {.fn I} for a single independent variable."
            ))
        }

        if (length(resp_data) != 1L) {
            cli::cli_abort(c(
                "{.arg resp} must be a single variable.",
                "i" = "Got {length(resp_data)} variable{?s}: {.val {names(resp_data)}}.",
                "i" = "Use a bare name or {.fn I} for a single response variable."
            ))
        }

        x_name = names(x_data)
        resp_name = names(resp_data)

        res = stats::cor.test(
            x = x_data[[1]],
            y = resp_data[[1]],
            method = .cor_type,
            alternative = .alt,
            conf.level = .ci
        )

        class_corr_two(
            ind_vars = x_name,
            resp_vars = resp_name,
            estimate = unname(res$estimate),
            statistic = unname(res$statistic),
            df = numeric(0),
            p_val = res$p.value,
            lower_ci = numeric(0),
            upper_ci = numeric(0),
            ci_level = .ci
        )
    }
}

pearson_fisher_z = function(x, y, ind_vars, resp_vars, rho, alt, ci) {
    n = length(x)

    if (n < 4L) {
        cli::cli_abort(c(
            "Fisher-z test requires at least 4 observations.",
            "i" = "Got {n}."
        ))
    }

    if (abs(rho) >= 1) {
        cli::cli_abort(c(
            "Hypothesized correlation must be strictly between -1 and 1.",
            "i" = "Got {.val {rho}}.",
            "i" = "Check whether the hypothesis expression or the supplied \".rho\" is written as intended."
        ))
    }

    r = stats::cor(x, y, method = "pearson")
    z_r = atanh(r)
    z_rho = atanh(rho)
    se = 1 / sqrt(n - 3)
    z_stat = (z_r - z_rho) / se

    p_val = switch(
        alt,
        "two.sided" = 2 * stats::pnorm(-abs(z_stat)),
        "greater" = stats::pnorm(z_stat, lower.tail = FALSE),
        "less" = stats::pnorm(z_stat)
    )

    alpha = 1 - ci
    z_crit = switch(
        alt,
        "two.sided" = stats::qnorm(1 - alpha / 2),
        "greater" = stats::qnorm(1 - alpha),
        "less" = stats::qnorm(1 - alpha)
    )

    lo_z = switch(
        alt,
        "two.sided" = z_r - z_crit * se,
        "greater" = z_r - z_crit * se,
        "less" = -Inf
    )
    up_z = switch(
        alt,
        "two.sided" = z_r + z_crit * se,
        "greater" = Inf,
        "less" = z_r + z_crit * se
    )

    class_corr_two(
        ind_vars = ind_vars,
        resp_vars = resp_vars,
        estimate = r,
        statistic = z_stat,
        df = numeric(0),
        p_val = p_val,
        lower_ci = tanh(lo_z),
        upper_ci = tanh(up_z),
        ci_level = ci
    )
}

#' @title Correlation Test: `rel` interface
#'
#' @description
#' The `rel` implementation performs a correlation test between exactly one
#' independent variable and one response variable.
#'
#' Use [rel()] as the model ID to select this implementation.
#'
#' @section Arguments:
#' The following arguments are passed via `...` in [CORTEST()]:
#'
#' \describe{
#'   \item{`.alt`}{String. One of `"two.sided"`, `"greater"`, or `"less"`.
#'     Default `"two.sided"`.}
#'   \item{`.ci`}{Numeric. Confidence level. Default `0.95`. Not applicable
#'     to Spearman and Kendall variants.}
#'   \item{`.rho`}{Numeric. Hypothesized population correlation coefficient
#'     under H\eqn{_0}. Default `0`. Only applicable to the `base` (Pearson)
#'     variant. When `0`, delegates to [stats::cor.test()]. When non-zero,
#'     uses a Fisher-z test against the specified null value.}
#' }
#'
#' @section Variants:
#' \describe{
#'   \item{`"spearman"`}{Spearman's \eqn{\rho}. Uses [stats::cor.test()] with
#'     `method = "spearman"`. No confidence interval is returned. Does not
#'     support [state_null()].}
#'   \item{`"kendall"`}{Kendall's \eqn{\tau}. Uses [stats::cor.test()] with
#'     `method = "kendall"`. No confidence interval is returned. Does not
#'     support [state_null()].}
#' }
#'
#' @section Correlation test default class:
#' Returns a [class_corr_two] object inheriting from [class_stat_infer].
#'
#' For the `base` variant, `df`, `lower_ci`, and `upper_ci` are always
#' populated. For `spearman` and `kendall`, those slots are `numeric(0)` and
#' are omitted from the printed output.
#'
#' @section Hypothesis claims:
#' Supports [RHO()] via [state_null()]. Only available on the `base`
#' (Pearson) variant. The claim is parsed as follows:
#'
#' - The operator maps to `.alt`: `==` and `!=` become `"two.sided"`,
#'   `>=` and `>` become `"less"`, `<=` and `<` become `"greater"`.
#' - The scalar maps to `.rho`: both `RHO(x, y) == 0.9` and
#'   `0.9 == RHO(x, y)` are handled correctly via [claim_scalar_diff()].
#'
#' @examples
#' # base (Pearson)
#' cars |>
#'     define_model(rel(speed, dist)) |>
#'     prepare_test(CORTEST) |>
#'     conclude()
#'
#' # Spearman
#' cars |>
#'     define_model(rel(speed, dist)) |>
#'     prepare_test(CORTEST) |>
#'     via("spearman") |>
#'     conclude()
#'
#' # Kendall
#' cars |>
#'     define_model(rel(speed, dist)) |>
#'     prepare_test(CORTEST) |>
#'     via("kendall") |>
#'     conclude()
#'
#' # hypothesis claim: two-sided against zero
#' cars |>
#'     define_model(rel(speed, dist)) |>
#'     prepare_test(CORTEST) |>
#'     state_null(RHO(speed, dist) == 0) |>
#'     conclude()
#'
#' # hypothesis claim: non-zero null, one-sided
#' cars |>
#'     define_model(rel(speed, dist)) |>
#'     prepare_test(CORTEST) |>
#'     state_null(RHO(speed, dist) >= 0.8) |>
#'     conclude()
#'
#' @keywords internal
#' @name cortest-rel
#' @family cortest-implementations
NULL

cor_test_rel = test_define(
    model_type = rel,
    impl = agendas(
        base = baseline(
            fn = function(.proc, .alt = "two.sided", .ci = 0.95, .rho = 0) {
                x_data = .proc$x_data
                resp_data = .proc$resp_data

                if (length(x_data) != 1L) {
                    cli::cli_abort(c(
                        "{.arg x} must be a single variable for {.fn rel}.",
                        "i" = "Got {length(x_data)} variable{?s}: {.val {names(x_data)}}.",
                        "i" = "Use a bare name or {.fn I} for a single independent variable."
                    ))
                }

                if (length(resp_data) != 1L) {
                    cli::cli_abort(c(
                        "{.arg resp} must be a single variable.",
                        "i" = "Got {length(resp_data)} variable{?s}: {.val {names(resp_data)}}.",
                        "i" = "Use a bare name or {.fn I} for a single response variable."
                    ))
                }

                x_name = names(x_data)
                resp_name = names(resp_data)
                x_vec = x_data[[1]]
                y_vec = resp_data[[1]]

                if (length(x_vec) < 4L) {
                    cli::cli_abort(c(
                        "No CI estimates generated. Correlation test requires at least 4 observations.",
                        "i" = "Got {length(x_vec)}."
                    ))
                }

                if (.rho == 0) {
                    res = stats::cor.test(
                        x = x_vec,
                        y = y_vec,
                        method = "pearson",
                        alternative = .alt,
                        conf.level = .ci
                    )

                    class_corr_two(
                        ind_vars = x_name,
                        resp_vars = resp_name,
                        estimate = unname(res$estimate),
                        statistic = unname(res$statistic),
                        df = unname(res$parameter),
                        p_val = res$p.value,
                        lower_ci = res$conf.int[[1]],
                        upper_ci = res$conf.int[[2]],
                        ci_level = .ci
                    )
                } else {
                    pearson_fisher_z(
                        x = x_vec,
                        y = y_vec,
                        ind_vars = x_name,
                        resp_vars = resp_name,
                        rho = .rho,
                        alt = .alt,
                        ci = .ci
                    )
                }
            }
        ),
        spearman = variant(
            fn = main_cortest_rel("spearman")
        ),
        kendall = variant(
            fn = main_cortest_rel("kendall")
        )
    ),
    compatible_params = list(RHO),
    claim_translator = claim_translate(
        default = map_claim(
            .alt = function(claim, processed) {
                switch(
                    claim@op,
                    "==" = , "!=" = "two.sided",
                    ">=" = , ">" = "less",
                    "<=" = , "<" = "greater"
                )
            },
            .rho = function(claim, processed) {
                resolved = claim_contrast_coefs(claim)
                coefs = resolved$coefs

                if (length(coefs) != 1L || coefs != 1) {
                    cli::cli_abort(c(
                        "Correlation test only supports a single {.fn RHO} parameter.",
                        "i" = "Found contrast coefficients: {.val {coefs}}.",
                        "i" = "{.fn RHO} cannot be scaled or combined with other parameters."
                    ))
                }

                resolved$scalar
            }
        )
    )
)
