#' Running conclusions with `conclude()`
#'
#' Executing the "lazy" pipeline to return the test you want to perform.
#'
#' @details
#' Everything is same as using \link[=eager-exec-test]{`run_test()`}, except you are allowed
#' to write the whole pipeline, which it directly executes the test implementation, and after
#' writing some recalibrations, e.g. using [via()] to switch from the classical method to
#' a certain type of method such as bootstrapping.
#'
#' @export
conclude = function(.x, ...) {
    UseMethod("conclude")
}

#' @rdname conclude
#' @export
conclude.test_lazy = function(.x, ...) {
    method_name = .x$recalibrate_spec$method_name %||% ""
    engine = .x$recalibrate_spec$engine %||%
        .x$engine %||%
        "default"
    model_type = class(.x$model_id)[[1]]

    def = find_def(
        .x$test_spec$lookup,
        model_type = model_type,
        method_name = method_name,
        engine = engine
    )

    method_args = if (!is.null(.x$recalibrate_spec)) {
        .x$recalibrate_spec$args
    } else {
        list()
    }

    context = infer_context(
        processed = .x$processed,
        args = .x$test_spec$args,
        extractors = def@vars,
        fun_args = def@fun_args,
        claims = .x$claims,
        method_args = method_args
    )

    out_raw = def@run(context)
    out = new_htest(
        out_raw,
        impl_cls = def@impl_class,
        test_cls = .x$test_spec$cls,
        def = def
    )
    out$name = .x$test_spec$name
    out
}

#' @rdname conclude
#' @export
conclude.engine_set = function(.x, ...) {
    if (!is.null(.x$recalibrate_spec)) {
        .x$recalibrate_spec$engine = .x$engine

        method_name = .x$recalibrate_spec$method_name
        engine = .x$engine
        model_type  = class(.x$model_id)[[1]]

        def = find_def(
            .x$test_spec$lookup,
            model_type = model_type,
            method_name = method_name,
            engine = engine
        )

        method_args = utils::modifyList(
            def@method@defaults,
            .x$recalibrate_spec$args
        )

        context = infer_context(
            processed = .x$processed,
            args = .x$test_spec$args,
            extractors = def@vars,
            claims = .x$claims,
            fun_args = def@fun_args,
            method_args = method_args
        )

        out_raw = def@run(context)
        out = new_htest(
            out_raw,
            impl_cls = def@impl_class,
            test_cls = .x$test_spec$cls,
            def = def
        )
        out$name = .x$test_spec$name
        return(out)
    }

    NextMethod()
}
