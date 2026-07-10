#' Automatically gauge effect size from a statistical result
#'
#' `auto_gauge()` is the protocol generic for computing effect-size
#' quantities from result objects produced by `fn` in `baseline()` and
#' `variant()`. It is called automatically by `gauge()` when the result
#' stored in `cld_exec@data` is a `class_stat_infer` subclass.
#'
#' Register a method on your output class to participate:
#'
#' ```r
#' S7::method(auto_gauge, my_test_result) = function(x, ...) {
#'     tibble::tibble(metric = "cohens_d", value = ...)
#' }
#' ```
#'
#' A variant whose `fn` returns the same result class as `baseline`
#' inherits `auto_gauge()` for free via S7's parent chain.
#'
#' @param x A `class_stat_infer` subclass object, typically `cld_exec@data`.
#' @param ... Currently unused. Passed to the dispatched method.
#'
#' @return A tibble with `metric` and `value` columns, one row per
#'   effect-size quantity.
#'
#' @seealso [gauge()], [making_gauge()], [method_gauge()], [class_stat_infer]
#'
#' @export
auto_gauge = S7::new_generic("auto_gauge", "x", fun = function(x, ...) {
    if (!is_class_stat_infer(x)) {
        cli::cli_abort(c(
            "{.arg x} must inherit {.cls class_stat_infer}.",
            "x" = "Got {.cls {class(x)[[1]]}}."
        ))
    }
    S7::S7_dispatch()
})

S7::method(auto_gauge, class_stat_infer) = function(x, ...) {
    cli::cli_abort(c(
        "No {.fn auto_gauge} method for {.cls {class(x)[[1]]}}.",
        "i" = "Implement {.fn auto_gauge} on your {.cls {class(x)[[1]]}} class."
    ))
}

S7::method(auto_gauge, class_lm_object) = function(x, ...) {
    y = x@fitted + x@residuals
    tss = sum((y - mean(y))^2)
    rss = x@deviance
    r2 = 1 - rss / tss
    f2 = r2 / (1 - r2)

    tibble::tibble(
        metric = c("r_squared", "cohens_f2"),
        value = c(r2, f2)
    )
}

S7::method(auto_gauge, class_glm_object) = function(x, ...) {
    deviance_r2 = 1 - x@deviance / x@null_deviance
    mcfadden_r2 = 1 - x@logLik / x@null_logLik

    tibble::tibble(
        metric = c(
            "deviance_r2",
            "mcfadden_r2",
            "cohens_f2_deviance",
            "cohens_f2_mcfadden"
        ),
        value = c(
            deviance_r2,
            mcfadden_r2,
            deviance_r2 / (1 - deviance_r2),
            mcfadden_r2 / (1 - mcfadden_r2)
        )
    )
}

S7::method(auto_gauge, class_p_test) = function(x, ...) {
    h = 2 * asin(sqrt(x@estimate)) - 2 * asin(sqrt(x@true_p %||% 0.5))
    tibble::tibble(metric = "cohens_h", value = h)
}

S7::method(auto_gauge, class_ttest_one) = function(x, ...) {
    n = x@df + 1
    d = x@t_stat / sqrt(n)
    tibble::tibble(
        term = x@term,
        metric = "cohens_d",
        value = d
    )
}

S7::method(auto_gauge, class_ttest_two) = function(x, quiet = TRUE, ...) {
    if (!quiet) {
        cli::cli_inform(c(
            "Cohen's d for two-sample t-tests is approximated as {.code 2 * t_stat / sqrt(df)}.",
            "i" = "This assumes roughly equal group sizes and may be inaccurate otherwise.",
            "i" = "Exact Cohen's d requires per-group sample sizes, not currently stored on {.cls ttest_two}.",
            "i" = "Pass {.code quiet = TRUE} to suppress this message."
        ))
    }
    d = 2 * x@t_stat / sqrt(x@df)
    tibble::tibble(
        group = x@group,
        metric = "cohens_d_approx",
        value = d
    )
}

S7::method(auto_gauge, class_ttest_pairwise) = function(x, quiet = TRUE, ...) {
    is_one_sample = x@var1 == x@var2

    if (!quiet && any(!is_one_sample)) {
        cli::cli_inform(c(
            "Cohen's d for two-sample pairs is approximated as {.code 2 * t_stat / sqrt(df)}.",
            "i" = "This assumes roughly equal group sizes per pair and may be inaccurate otherwise.",
            "i" = "Pass {.code quiet = TRUE} to suppress this message."
        ))
    }

    n = x@df + 1
    d = ifelse(
        is_one_sample,
        x@t_stat / sqrt(n),
        2 * x@t_stat / sqrt(x@df)
    )

    tibble::tibble(
        var1 = x@var1,
        var2 = x@var2,
        metric = ifelse(is_one_sample, "cohens_d", "cohens_d_approx"),
        value = d
    )
}
