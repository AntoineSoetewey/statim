#' T-Test
#'
#' `TTEST()` performs a t-test for one-sample, two-sample, paired, pairwise, or
#' formula-based comparisons. If `TTEST` is supplied within the lazy-loaded pipeline,
#' supply `TTEST` as a function within i.e. `prepare_test(.test = TTEST)` call.
#'
#' @param .model A model ID from `x_by()`, `pairwise()`, or a formula.
#'   When supplied, the test executes immediately.
#' @param .data A data frame. Only used on the standalone path.
#' @param ... Additional arguments passed to the implementation. See the
#'   **Arguments by model ID** section for the full list per path.
#'
#' @return A `cld_exec` object (in [conclude()]), a `stat_infer_spec` object, or a
#'   `test_spec` when `.model = NULL`. Depending on the implementation you wrote, it returns
#'   any class. However, by default, some implementations use base `{statim}` S7 classes.
#'   For instance:
#'   - `ttest_x_by`, by default, returns a [class_ttest_two] object
#'   - `ttest_pairwise`, by default, returns a [class_ttest_pairwise] object
#'
#' @section Supported model IDs:
#' Each model ID routes to a separate implementation. See the linked pages
#' for full argument lists, variants, and result class details:
#'
#' - `x_by()`: two-sample or paired t-test. See [ttest-xby].
#' - `pairwise()`: pairwise t-tests across variables. See [ttest-pairwise].
#' - `<formula>`: one-sample and/or two-sample t-test. See [ttest-formula].
#'
#' @inheritSection ttest-xby Arguments
#' @inheritSection ttest-xby Variants
#' @inheritSection ttest-xby Result class
#' @inheritSection ttest-xby Hypothesis claims
#'
#' @examples
#' # eager
#' TTEST(x_by(extra, group), sleep)
#'
#' # pipeline
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' # bootstrap
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     via("boot", n = 2000) |>
#'     conclude()
#'
#' # permutation
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     via("permute", n = 2000) |>
#'     conclude()
#'
#' # Contrast t-test
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     state_null(
#'         2 * MU(extra, group == "1") <= MU(extra, group == "2")
#'     ) |>
#'     # Try to obtain 90% of the confidence interval
#'     via("contrast", .ci = 0.9) |>
#'     conclude()
#'
#' # pairwise
#' iris |>
#'     define_model(pairwise(Sepal.Length, Sepal.Width, Petal.Length)) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' # hypothesis claim
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     state_null(MU(extra) == 0) |>
#'     conclude()
#'
#' @seealso
#' [ttest-xby], [ttest-pairwise], [ttest-formula] for per-implementation
#' details. [class_ttest_two], [class_ttest_pairwise] for result class
#' slots. [via()], [state_null()], [conclude()], [auto_tidy()].
#'
#' @export
TTEST = HTEST_FN(
    cls = "ttest",
    defs = list(
        ttest_def_two,
        ttest_def_formula,
        ttest_def_pairwise
    ),
    .name = "T-Test"
)

#' Structured result container for two-sample t-tests
#'
#' @description
#' An S7 class produced by [TTEST] pipelines using [x_by()] as the model ID.
#' Not constructed manually — use the pipeline instead.
#'
#' Inherits from [class_stat_infer], so [auto_tidy()] dispatches on it
#' automatically. Downstream packages can use it as a `parent` in
#' `S7::new_class()`.
#'
#' @usage NULL
#'
#' @details
#' Slots (populated automatically by [TTEST]):
#'
#' - `group`: name of the grouping variable.
#' - `estimate`: mean difference (or linear contrast estimate).
#' - `t_stat`: t-statistic.
#' - `df`: degrees of freedom.
#' - `p_val`: p-value.
#' - `lower_ci`: lower confidence bound.
#' - `upper_ci`: upper confidence bound.
#' - `ci_level`: confidence level, e.g. `0.95`.
#'
#' @section Shared by variants:
#' Both the default (`base`) and `contrast` return a `class_ttest_two`, so
#' [auto_tidy()] and [print()] are inherited by `contrast` for free.
#'
#' @seealso [TTEST], [auto_tidy()], [class_stat_infer]
#'
#' @export
class_ttest_two = S7::new_class(
    "ttest_two",
    parent = class_stat_infer,
    properties = list(
        group = S7::class_character,
        estimate = S7::class_numeric,
        t_stat = S7::class_numeric,
        df = S7::class_numeric,
        p_val = S7::class_numeric,
        lower_ci = S7::class_numeric,
        upper_ci = S7::class_numeric,
        ci_level = S7::new_property(
            class = S7::class_numeric,
            default = 0.95,
            validator = function(value) {
                if (length(value) != 1L)
                    return(paste0("`ci_level` must be length 1, not ", length(value), "."))
                if (value <= 0 || value >= 1)
                    "`ci_level` must be between 0 and 1 (exclusive)."
            }
        )
    )
)

S7::method(print, class_ttest_two) = function(x, ...) {
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
        group = x@group,
        estimate = round(x@estimate, 4),
        t_stat = round(x@t_stat, 4),
        df = round(x@df, 2),
        p_val = round(x@p_val, 4)
    )

    ci_out = tibble::tibble(
        group = x@group,
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

#' Structured result container for pairwise t-tests
#'
#' @description
#' An S7 class produced by [TTEST] pipelines using [pairwise()] as the
#' model ID. Not constructed manually — use the pipeline instead.
#'
#' Inherits from [class_stat_infer], so [auto_tidy()] dispatches on it
#' automatically. Downstream packages can use it as a `parent` in
#' `S7::new_class()`.
#'
#' @usage NULL
#'
#' @details
#' Slots (populated automatically by [TTEST]):
#'
#' - `var1`: first variable in each pair.
#' - `var2`: second variable in each pair.
#' - `est`: mean difference per pair (or sample mean for one-sample mode).
#' - `df`: degrees of freedom per pair.
#' - `t_stat`: t-statistic per pair.
#' - `p_value`: p-value per pair.
#' - `method_name`: scalar string describing the test method, taken directly
#'   from [stats::t.test()]. Must be length 1 — all pairs must share the
#'   same method.
#'
#' @section One-sample mode:
#' When [pairwise()] uses `direction = "eq"`, `var1` and `var2` are
#' identical (each variable tested against itself). [print()] detects this
#' and renders a diagonal-only matrix.
#'
#' @seealso [TTEST], [ttest-pairwise], [auto_tidy()], [class_stat_infer]
#'
#' @export
class_ttest_pairwise = S7::new_class(
    "ttest_pairwise",
    parent = class_stat_infer,
    properties = list(
        var1 = S7::class_character,
        var2 = S7::class_character,
        est = S7::class_numeric,
        df = S7::class_numeric,
        t_stat = S7::class_numeric,
        p_value = S7::class_numeric,
        method_name = S7::new_property(
            class = S7::class_character,
            default = "",
            validator = function(value) {
                if (length(value) != 1L)
                    paste0("`method_name` must be length 1, not ", length(value), ".")
            }
        )
    )
)

S7::method(print, class_ttest_pairwise) = function(x, ...) {
    is_one_sample = all(x@var1 == x@var2)

    if (is_one_sample) {
        vars = x@var1
        grid = expand.grid(var1 = vars, var2 = vars, stringsAsFactors = FALSE)

        diff_vec = rep("", nrow(grid))
        t_vec = rep("", nrow(grid))
        pval_vec = rep("", nrow(grid))

        for (k in seq_along(vars)) {
            idx = which(grid$var1 == vars[[k]] & grid$var2 == vars[[k]])
            diff_vec[[idx]] = formatC(x@est[[k]], digits = 3, format = "f")
            t_vec[[idx]] = formatC(x@t_stat[[k]], digits = 3, format = "f")
            pval_vec[[idx]] = formatC(x@p_value[[k]], digits = 3, format = "f")
        }

        spec = tabstats::new_pairwise_data(
            var1 = grid$var1,
            var2 = grid$var2,
            diff = diff_vec,
            t_stat = t_vec,
            pval = pval_vec
        )
    } else {
        spec = tabstats::new_pairwise_data(
            var1 = x@var1,
            var2 = x@var2,
            diff = formatC(x@est, digits = 3, format = "f"),
            t_stat = formatC(x@t_stat, digits = 3, format = "f"),
            pval = formatC(x@p_value, digits = 3, format = "f")
        )
    }

    tabstats::pairwise_matrix(
        spec,
        title = if (nzchar(x@method_name)) x@method_name else "Pairwise t-Tests",
        layout_view = TRUE,
        diag_1 = FALSE,
        style = tabstats::cm_style(
            pval = function(x) {
                x_num = suppressWarnings(as.numeric(x))
                if (is.na(x_num) || x_num > 0.05) {
                    cli::style_italic(x)
                } else if (x_num > 0.01) {
                    cli::col_red(x)
                } else {
                    cli::style_bold("<0.001")
                }
            }
        )
    )

    invisible(x)
}
