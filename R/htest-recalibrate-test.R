#' Recalibrate H-test pipeline with `via()`
#'
#' @export
via = function(.x, .method, ...) {
    UseMethod("via")
}

#' @rdname via
#' @export
via.test_lazy = function(.x, .method, ..., engine = "default") {
    dots = list(...)

    key = paste0(class(.x$model_id)[[1]], "::", .method, "::", engine)
    def = .x$test_spec$lookup[[key]] %||% cli::cli_abort(
        "No implementation for method {.val {(.method)}}."
    )

    method_args = utils::modifyList(
        def@method@defaults,
        dots
    )

    .x$recalibrate_spec = list(
        method_name = .method,
        engine = engine,
        args = method_args
    )
    .x
}
