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

S7::method(
    auto_gauge,
    S7::new_union(class_lm_object, class_glm_object)
) = function(x, ...) {
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

S7::method(auto_gauge, class_p_test) = function(x, ...) {
    h = 2 * asin(sqrt(x@estimate)) - 2 * asin(sqrt(x@true_p %||% 0.5))
    tibble::tibble(metric = "cohens_h", value = h)
}
