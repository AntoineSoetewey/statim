#' Eager execution of the tests
#'
#' @description
#' This eagerly executes the test
#'
#' @details
#' You are allowed to run an H-test function, e.g. through `TTEST(extra ~ group, sleep)`,
#' eagerly. Under the hood, `defs` contains the list of implementations, construct a dictionary of
#' implemented functions with `build_lookup()`, then match it with `find_def()`. The impl. being
#' look-up is saved as `def`, and it's `S7` class, not `S3` nor `S4`.
#'
#' Since this eagerly executes the test, it won't try to rely on `define_model()` to process
#' the `model_id` being defined. It has to be processed directly (thus, `processed = model_processor(model_id, data = .data)`
#' on the third line of code).
#'
#' Internally, the context of the test is then lookup by `infer_context` R6 class, because
#' this intends to pass on the arguments being used from the implementation. Then, use the
#' constructed context under `def@run()` to execute the test you want to perform.
#'
#' Save the raw output into a new class, under `new_htest()` typically.
#'
#' The `.name` will be the name of the test, and it's optional actually.
#'
#' @keywords internal
#' @rdname eager-exec-test
run_htest = function(defs, args, cls, model_id, .data, .name) {
    lookup = build_lookup(defs)
    def = find_def(
        lookup,
        model_type = class(model_id)[[1]],
        method_name = ""
    )
    processed = model_processor(model_id, data = .data)

    context = infer_context(
        processed = processed,
        args = args,
        extractors = def@vars,
        claims = NULL,
        method_args = list()
    )

    out_raw = def@run(context)
    out = new_htest(
        out_raw,
        impl_cls = def@impl_class,
        test_cls = cls,
        def = def
    )
    out$name = .name
    out
}

#' Deferred execution of the tests
#'
#' @description
#' This is looked up under the main pipeline.
#'
#' @keywords internal
#' @noRd
defer_htest = function(lookup, args, cls, defs, .name) {
    out = list(
        defs = defs,
        args = args,
        cls = cls,
        name = .name,
        lookup = lookup
    )
    class(out) = "test_spec"
    out
}

build_htest = function(defs, args, cls, model_id, .data = NULL, .name) {
    lookup = build_lookup(defs)

    if (!is.null(model_id)) {
        run_htest(lookup, args, cls, model_id, .data, .name)
    } else {
        defer_htest(lookup, args, cls, defs, .name)
    }
}

#' H-test specs S3 class vector
#'
#' This wraps raw results of the tests
#'
#' @keywords internal
#' @noRd
new_htest = function(res, impl_cls, test_cls, def = NULL) {
    out = list(data = res)
    class(out) = c(impl_cls, test_cls, "htest_spec")
    attr(out, "print_fn") = if (!is.null(def)) def@print else NULL
    out
}

build_lookup = function(defs) {
    keys = vapply(defs, function(d) {
        method_name = if (is.null(d@method)) "" else d@method@method_name
        paste0(d@model_type, "::", method_name, "::", d@engine)
    }, character(1))
    rlang::set_names(defs, keys)
}

find_def = function(lookup, model_type, method_name = "", engine = "default") {
    key = paste0(model_type, "::", method_name, "::", engine)
    lookup[[key]] %||% cli::cli_abort(
        "No implementation found for {.val {key}}."
    )
}

#' @export
print.htest_spec = function(x, ...) {
    def_print = attr(x, "print_fn")
    if (!is.null(def_print)) {
        def_print(x, ...)
    } else {
        print(x$data)
    }
    invisible(x)
}

#' Build a hypothesis test function
#'
#' `new_htest_fn()` is a developer-interface function, a constructor for user-facing
#' test functions like [TTEST()]. It returns a function with a consistent
#' signature that routes to the correct implementation based on the model ID
#' and method variant.
#'
#' @param cls A string naming the test class, e.g. `"ttest"`.
#' @param defs A list of `test_define` objects declaring the implementations.
#' @param .name A string used as the test title in output.
#'
#' @return A function with signature
#'   `function(.model, .data, ..., .extra_defs)`.
#'
#' @seealso [test_define()], [prepare_test()], [via()], [conclude()]
#'
#' @examples
#' MY_TEST = new_htest_fn(
#'     cls = "mytest",
#'     defs = list(my_def_two),
#'     .name = "My Test"
#' )
#'
#' @export
HTEST_FN = function(cls, defs, .name) {
    force(cls)
    force(defs)
    force(.name)

    function(.model = NULL, .data = NULL, ..., .extra_defs = list()) {
        build_htest(
            cls = cls,
            args = list(...),
            defs = c(defs, .extra_defs),
            model_id = .model,
            .data = .data,
            .name = .name
        )
    }
}
