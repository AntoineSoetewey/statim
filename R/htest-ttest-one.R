#' Structured result container for one-sample t-tests
#'
#' @description
#' An S7 class produced by [T_TEST] pipelines using [on()] as the variable
#' mapper `<var_id>`. Not constructed manually — use the pipeline instead.
#'
#' Inherits from [class_stat_infer], so [auto_tidy()] dispatches on it
#' automatically. Downstream packages can use it as a `parent` in
#' `S7::new_class()`.
#'
#' @usage NULL
#'
#' @details
#' Slots (populated automatically by [T_TEST]):
#'
#' - `term`: name of the tested variable.
#' - `estimate`: sample mean.
#' - `true_mu`: hypothesized mean as written in the claim. Falls back to
#'   `.mu` when no [state_null()] claim is supplied.
#' - `statistic`: t-statistic.
#' - `p_val`: p-value.
#' - `lower_ci`: lower confidence bound.
#' - `upper_ci`: upper confidence bound.
#' - `ci_level`: confidence level, e.g. `0.95`.
#'
#' @section Shared by variants:
#' Both `base` and `multi` return a `class_ttest_one`, so [auto_tidy()] and
#' [print()] are inherited by `multi` for free.
#'
#' @returns An S7 object of class `ttest_one`, with the properties listed in
#' Details. Not constructed manually; returned by [T_TEST] pipelines.
#'
#' @seealso [T_TEST], [ttest-on], [auto_tidy()], [class_stat_infer]
#'
#' @export
class_ttest_one = S7::new_class(
    "ttest_one",
    parent = class_stat_infer,
    properties = list(
        term = S7::class_character,
        estimate = S7::class_numeric,
        true_mu = S7::class_numeric,
        df = S7::class_numeric,
        t_stat = S7::class_numeric,
        p_val = S7::class_numeric,
        lower_ci = S7::class_numeric,
        upper_ci = S7::class_numeric,
        ci_level = S7::new_property(
            class = S7::class_numeric,
            default = 0.95,
            validator = function(value) {
                if (length(value) != 1L) {
                    return(paste0(
                        "`ci_level` must be length 1, not ",
                        length(value),
                        "."
                    ))
                }
                if (value <= 0 || value >= 1) {
                    "`ci_level` must be between 0 and 1 (exclusive)."
                }
            }
        )
    )
)

S7::method(print, class_ttest_one) = function(x, ...) {
    ci_level = x@ci_level * 100
    lo_name = paste0("lower_", ci_level)
    up_name = paste0("upper_", ci_level)

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

    fmt_ci = function(val) {
        ifelse(is.infinite(val), ifelse(val > 0, "Inf", "-Inf"), round(val, 4))
    }

    stat_out = tibble::tibble(
        term = x@term,
        estimate = round(x@estimate, 4),
        true_mu = x@true_mu,
        t_stat = round(x@t_stat, 4),
        p_val = round(x@p_val, 4)
    )

    ci_out = tibble::tibble(
        term = x@term,
        !!lo_name := fmt_ci(x@lower_ci),
        !!up_name := fmt_ci(x@upper_ci)
    )

    cli::cat_line(cli::rule(left = "Summary", line = "-"), "\n")
    tabstats::table_default(
        stat_out,
        style_columns = tabstats::td_style(p_val = pval_styler)
    )
    cat("\n\n")

    cli::cat_line(cli::rule(left = "Confidence Interval", line = "-"), "\n")
    tabstats::table_default(ci_out)
    cat("\n\n")

    invisible(x)
}
