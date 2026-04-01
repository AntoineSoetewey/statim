#' Lazily prepare a single test
#'
#' `prepare_test()` registers a single test for lazy execution. Use this
#' when you intend to call `plot_test()` after `run_test()`.
#'
#' @param .x A `stat_infer_pipeline` object.
#' @param test A single test specification from a test function like `TTEST()`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `stat_infer_pipeline` object.
#'
#' @name prepare-test
#' @export
prepare_test = function(.x, .test, ...) {
    UseMethod("prepare_test")
}

#' @rdname prepare-test
#' @export
prepare_test.def_model = function(.x, .test, ...) {
    spec = as_test_spec(.test)

    out = list(
        model_id = .x$model_id,
        processed = .x$processed,
        test_spec = spec,
        replicate_spec = NULL,
        claims = NULL
    )
    class(out) = "test_lazy"
    out
}

#' @importFrom stats update
#' @export
update.test_lazy = function(object, ...) {
    dots = list(...)
    if (!is.null(object$recalibrate_spec)) {
        object$recalibrate_spec$args = utils::modifyList(
            object$recalibrate_spec$args, dots
        )
    } else {
        object$test_spec$args = utils::modifyList(
            object$test_spec$args, dots
        )
    }
    object
}

as_test_spec = function(.test) {
    if (inherits(.test, "test_spec")) return(.test)

    if (is.function(.test)) {
        spec = .test(.model = NULL)
        if (!inherits(spec, "test_spec"))
            cli::cli_abort("{.arg .test} must return a {.cls test_spec}.")
        return(spec)
    }

    cli::cli_abort("{.arg .test} must be a function or {.cls test_spec}.")
}

