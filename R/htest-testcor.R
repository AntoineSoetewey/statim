#' Correlation Test
#'
#' `COR_TEST()` performs a correlation test for one-to-one variable
#' relationships. If `COR_TEST` is supplied within the lazy-loaded pipeline,
#' supply `COR_TEST` as a function i.e. `prepare_test(.test = COR_TEST)` call.
#'
#' @param .var_id A variable mapper `<var_id>` for `COR_TEST()`, e.g. [rel()].
#'   When supplied, the test executes immediately.
#' @param .data A data frame. Only used on the standalone path.
#' @param ... Additional arguments passed to the implementation.
#'   See the **Arguments** section of each implementation page.
#'
#' @return A `cld_exec` object (in [conclude()]), or a `test_spec` object
#'   when `.var_id = NULL`. The default correlation test class for most paths is
#'   [class_corr_two].
#'
#' @section Supported variable mapper `<var_id>`s:
#' Each variable mapper `<var_id>` routes to a separate implementation. See the linked pages
#' for full argument lists, variants, and correlation test class details:
#'
#' - `rel()`: one-to-one correlation test. See details from [cortest-rel].
#' - `<formula>`: one-to-many correlation test. See details from [cortest-formula].
#'
#' @examples
#' # eager
#' COR_TEST(rel(speed, dist), cars)
#'
#' # grammatical syntax
#' cars |>
#'     define_model(rel(speed, dist)) |>
#'     prepare_test(COR_TEST) |>
#'     conclude()
#'
#' cars |>
#'     define_model(speed ~ dist) |>
#'     prepare_test(COR_TEST) |>
#'     conclude()
#'
#' # Spearman
#' suppressWarnings({
#'     cars |>
#'         define_model(rel(speed, dist)) |>
#'         prepare_test(COR_TEST) |>
#'         via("spearman") |>
#'         conclude()
#' })
#'
#' # Custom Hypothesis Expression
#' cars |>
#'     define_model(rel(speed, dist)) |>
#'     prepare_test(COR_TEST) |>
#'     state_null(RHO(speed, dist) >= 0.8) |>
#'     conclude()
#'
#' @seealso
#' [cortest-rel], [cortest-formula] for per-implementation details.
#' [class_corr_two] for correlation test class slots.
#' [via()], [state_null()], [conclude()], [auto_tidy()].
#'
#' @export
COR_TEST = HTEST_FN(
    cls = "cortest",
    defs = list(
        cor_test_rel,
        cor_test_formula
        # cor_test_pairwise
    ),
    .name = "Correlation Test"
)

#' Structured result container for two-sample t-tests
#'
#' @description
#' An S7 class produced by [COR_TEST] using [rel()] and `<formula>` as the variable mapper `<var_id>`.
#' Not constructed manually, use the "grammar interface" instead.
#'
#' Inherits from [class_stat_infer], so [auto_tidy()] dispatches on it
#' automatically. Downstream packages can use it as a `parent` in
#' `S7::new_class()`.
#'
#' @usage NULL
#'
#' @details
#' Slots (populated automatically by [COR_TEST]):
#'
#' - `ind_vars`: name of the independent variables.
#' - `resp_vars`: name of the response / dependent variables.
#' - `estimate`: the estimated correlation coefficient.
#' - `statistic`: t-statistic.
#' - `df`: degrees of freedom.
#' - `p_val`: p-value.
#' - `lower_ci`: lower confidence bound.
#' - `upper_ci`: upper confidence bound.
#' - `ci_level`: confidence level, e.g. `0.95`.
#'
#' @section Shared by variants:
#' Both [rel()] and `<formula>`'s default (`base`) return a `class_corr_two`, so different
#' models shares both [auto_tidy()] and [print()] for free.
#'
#' @seealso [COR_TEST], [auto_tidy()], [class_stat_infer]
#'
#' @export
class_corr_two = S7::new_class(
    "corr_two",
    parent = class_stat_infer,
    properties = list(
        ind_vars = S7::class_character,
        resp_vars = S7::class_character,
        pairs = S7::new_property(default = NULL, getter = function(self) {
            paste(self@resp_vars, "~", self@ind_vars)
        }),
        estimate = S7::class_numeric,
        statistic = S7::class_numeric,
        df = S7::new_property(class = S7::class_numeric, default = numeric(0)),
        p_val = S7::class_numeric,
        lower_ci = S7::new_property(
            class = S7::class_numeric,
            default = numeric(0)
        ),
        upper_ci = S7::new_property(
            class = S7::class_numeric,
            default = numeric(0)
        ),
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

S7::method(print, class_corr_two) = function(x, ...) {
    ci_level = x@ci_level * 100
    lo_name = paste0("lower_", ci_level)
    up_name = paste0("upper_", ci_level)
    has_df = length(x@df) > 0L
    has_ci = length(x@lower_ci) > 0L

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

    stat_out = tibble::tibble(
        pair = x@pairs,
        estimate = round(x@estimate, 4),
        statistic = round(x@statistic, 4),
        p_val = round(x@p_val, 4)
    )

    if (has_df) {
        stat_out = tibble::add_column(
            stat_out,
            df = round(x@df, 2),
            .after = "statistic"
        )
    }

    cli::cat_line(cli::rule(left = "Summary", line = "-"), "\n")
    tabstats::table_default(
        stat_out,
        style_columns = tabstats::td_style(p_val = pval_styler)
    )
    cat("\n\n")

    if (has_ci) {
        fmt_ci = function(val) {
            ifelse(
                is.infinite(val),
                ifelse(val > 0, "Inf", "-Inf"),
                round(val, 4)
            )
        }

        ci_out = tibble::tibble(
            pair = x@pairs,
            !!lo_name := fmt_ci(x@lower_ci),
            !!up_name := fmt_ci(x@upper_ci)
        )

        cli::cat_line(cli::rule(left = "Confidence Interval", line = "-"), "\n")
        tabstats::table_default(ci_out)
        cat("\n\n")
    }

    invisible(x)
}
