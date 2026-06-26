#' Prepare a lazy inference pipeline
#'
#' `prepare()` attaches a spec function produced from [STAT_CONSTRUCTOR()] to a
#' `<def_var>` object and dispatches to [prepare_test()] or [prepare_model()]
#' depending on whether `.fn` returns a `<test_spec>` or a `<model_spec>`.
#' The result is a lazy pipeline object ready for optional recalibration
#' with [via()] before execution with [conclude()].
#'
#' @param .x A `<def_var>` object from [define_model()], or an
#'   `<expanded_model>` object from [write_models()].
#' @param .fn A stat function built with [STAT_CONSTRUCTOR()] that returns
#'   a `<test_spec>` (e.g. [TTEST]) or a `<model_spec>` (e.g. [LINEAR_REG]).
#' @param ... Additional arguments passed to the dispatched `prepare_*()` function.
#'
#' @return A `<test_lazy>` object if `.fn` returns a `<test_spec>`, or a
#'   `<model_lazy>` object if `.fn` returns a `<model_spec>`.
#'
#' @seealso [prepare_test()], [prepare_model()], [define_model()], [via()], [conclude()]
#'
#' @examples
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare(TTEST) |>
#'     conclude()
#'
#' mtcars |>
#'     define_model(mpg ~ .) |>
#'     prepare(LINEAR_REG) |>
#'     conclude()
#'
#' @export
prepare = S7::new_generic("prepare", c(".x", ".fn"))

is_test_spec = function(x) S7::S7_inherits(x, test_spec)
is_model_spec = function(x) S7::S7_inherits(x, model_spec)

S7::method(prepare, list(def_var, S7::class_function)) = function(.x, .fn, ...) {
    spec = tryCatch(
        .fn(.var_id = NULL),
        error = function(e) {
            cli::cli_abort(
                "{.arg .fn} must be a function built with {.fn STAT_CONSTRUCTOR}.",
                parent = e
            )
        }
    )

    if (is_test_spec(spec)) {
        test_lazy(
            var_id = .x@var_id,
            processed = .x@processed,
            test_spec = spec,
            recalibrate_spec = if (length(list(...)) > 0L) list(args = list(...)) else NULL
        )
    } else if (is_model_spec(spec)) {
        model_lazy(
            var_id = .x@var_id,
            processed = .x@processed,
            model_spec = spec,
            recalibrate_spec = if (length(list(...)) > 0L) list(args = list(...)) else NULL
        )
    } else {
        cli::cli_abort("{.arg .fn} must return a {.cls test_spec} or {.cls model_spec}.")
    }
}
