#' Automatically tidy a statistical result
#'
#' `auto_tidy()` is the protocol generic for tidying result objects produced
#' by `fn` in [baseline()] and [variant()]. It is called automatically by
#' [tidy()] when the result stored in `cld_exec@data` is a [class_stat_infer]
#' subclass.
#'
#' Register a method on your output class to participate in the protocol:
#'
#' ```r
#' example_out = S7::new_class("example_out", parent = class_stat_infer)
#'
#' S7::method(auto_tidy, example_out) = function(x, ...) {
#'     tibble::tibble(...)
#' }
#' ```
#'
#' When a variant's `fn` returns the same output class as `baseline`, it
#' inherits `auto_tidy()` automatically via S7's parent chain. When it
#' returns a subclass, it can override selectively:
#'
#' ```r
#' new_boot_class = S7::new_class("new_boot_class", parent = example_out)
#'
#' # override only for boot
#' # everything else inherited from `example_out`
#' S7::method(auto_tidy, new_boot_class) = function(x, ...) {
#'     tibble::tibble(...)
#' }
#' ```
#'
#' If no `auto_tidy()` method is found and no [making_tidy()] entry exists,
#' [tidy()] falls back to an informative error.
#'
#' @param x A [class_stat_infer] subclass object, typically `cld_exec@data`.
#' @param ... Currently unused. Passed to the dispatched method.
#'
#' @return A tibble.
#'
#' @seealso [tidy()], [making_tidy()], [method_tidy()], [class_stat_infer]
#'
#' @export
auto_tidy = S7::new_generic("auto_tidy", "x", fun = function(x, ...) {
    if (!is_class_stat_infer(x)) {
        cli::cli_abort(c(
            "{.arg x} must inherit {.cls class_stat_infer}.",
            "x" = "Got {.cls {class(x)[[1]]}}."
        ))
    }
    S7::S7_dispatch()
})

S7::method(auto_tidy, class_stat_infer) = function(x, ...) {
    cli::cli_abort(c(
        "No {.fn auto_tidy} method for {.cls {class(x)[[1]]}}.",
        "i" = "Implement {.fn auto_tidy} on your {.cls {class(x)[[1]]}} class."
    ))
}

S7::method(
    auto_tidy,
    S7::new_union(class_lm_object, class_glm_object)
) = function(x, ...) {
    tibble::tibble(x@coefficients)
}

S7::method(auto_tidy, class_ttest_two) = function(x, ...) {
    ci_level = x@ci_level * 100
    lo_name = paste0("lower_", ci_level)
    up_name = paste0("upper_", ci_level)

    tibble::tibble(
        group = x@group,
        estimate = x@estimate,
        t_stat = x@t_stat,
        df = x@df,
        p_val = x@p_val,
        !!lo_name := x@lower_ci,
        !!up_name := x@upper_ci
    )
}

S7::method(auto_tidy, class_p_test) = function(x, ...) {
    ci_level = x@ci_level * 100
    has_true_p = length(x@true_p) > 0L

    out = tibble::tibble(
        successes = x@x,
        total = x@n,
        estimate = x@estimate,
        statistic = x@statistic,
        p_val = x@p_val
    )

    if (has_true_p) {
        out = tibble::add_column(out, true_p = x@true_p, .after = "total")
    }

    lo_name = paste0("lower_", ci_level)
    up_name = paste0("upper_", ci_level)
    out[[lo_name]] = x@lower_ci
    out[[up_name]] = x@upper_ci
    out
}

S7::method(auto_tidy, class_corr_two) = function(x, ...) {
    ci_level = x@ci_level * 100
    has_df = length(x@df) > 0L
    has_ci = length(x@lower_ci) > 0L

    out = tibble::tibble(
        pair = x@pairs,
        estimate = x@estimate,
        statistic = x@statistic,
        p_val = x@p_val
    )

    if (has_df) {
        out = tibble::add_column(out, df = x@df, .after = "statistic")
    }

    if (has_ci) {
        lo_name = paste0("lower_", ci_level)
        up_name = paste0("upper_", ci_level)
        out[[lo_name]] = x@lower_ci
        out[[up_name]] = x@upper_ci
    }

    out
}
