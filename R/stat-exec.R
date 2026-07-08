#' Execute a lazy pipeline
#'
#' `conclude()` is the terminal step of the pipeline. It resolves the
#' method variant, runs the implementation, and returns a `cld_exec` S7 object.
#'
#' @param .x A `test_lazy` or `model_lazy` object produced by
#'   [prepare_test()] or [prepare_model()] (optionally followed by [via()]).
#' @param ... Currently unused.
#'
#' @return A `cld_exec` S7 object with the following slots:
#'   \describe{
#'     \item{`@data`}{The raw return value of the `fn` defined in [baseline()]
#'       or [variant()]. Its structure depends on the implementation — see the
#'       documentation of the stat function (e.g. `?T_TEST`) for what to expect.}
#'     \item{`@cld_meta`}{A list of pipeline metadata:
#'       \describe{
#'         \item{`$var_id`}{The Variable Mapper object passed to [define_model()].}
#'         \item{`$processed`}{The processed model output from [model_processor()].
#'           The same object received as `.proc` inside the `fn`.}
#'         \item{`$stat_name`}{The human-readable test or model name.}
#'         \item{`$method`}{The variant name used. `"default"` when no [via()]
#'           was called.}
#'         \item{`$data_name`}{The name of the data frame, if resolvable.}
#'       }
#'     }
#'   }
#'
#' @section Writing print functions:
#' The `print` argument of [baseline()] and [variant()] receives a `cld_exec`
#' object as `x`. Read your output from `x@data`:
#'
#' ```r
#' baseline(
#'     fn = function(.proc, .mu = 0) { ... },
#'     print = function(x, ...) {
#'         dat = x@data
#'         # render dat
#'         invisible(x)
#'     }
#' )
#' ```
#'
#' Otherwise, when the base S7 class dispatches `print()` elsewhere, it is inherited
#' without writing `print` from [baseline()] / [variant()]
#'
#' @section Writing tidy functions:
#' Prefer implementing [auto_tidy()] on your result class when `fn` returns
#' a [class_stat_infer] subclass. Use [making_tidy()] only when `fn`
#' intentionally returns a non-[class_stat_infer] object.
#'
#' For example:
#'
#' ```r
#' making_tidy(T_TEST, x_by) %<-% method_tidy(
#'     default = function(.x, ...) {
#'         dat = .x@data
#'         # return a tibble
#'     }
#' )
#' ```
#'
#' @seealso [prepare_test()], [prepare_model()], [via()], [model_processor()],
#'   [class_stat_infer], [auto_tidy()]
#'
#' @examples
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(T_TEST) |>
#'     conclude()
#'
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(T_TEST) |>
#'     via("boot", n = 2000) |>
#'     conclude()
#'
#' mtcars |>
#'     define_model(rel(mpg, wt)) |>
#'     prepare_model(LINEAR_REG) |>
#'     conclude()
#'
#' @name conclude
#' @export
conclude = S7::new_generic("conclude", ".x")

S7::method(conclude, test_lazy) = function(.x, ...) {
    model_type = if (inherits(.x@var_id, "formula")) {
        "formula"
    } else {
        S7::S7_class(.x@var_id)@name
    }
    cls = .x@test_spec@cls
    lookup = if (
        identical(.x@test_spec@registry_version, stat_define_registry$.version)
    ) {
        .x@test_spec@lookup
    } else {
        build_lookup(.x@test_spec@defs, cls)
    }
    def = find_def(lookup, model_type = model_type)

    # method_name = .x@recalibrate_spec$method_name
    # cls = .x@test_spec@cls
    method_name = .x@recalibrate_spec$method_name
    key = variant_registry_key(cls, model_type)
    impl = def@impl$variants[[method_name %||% ""]] %||%
        variant_registry[[key]][[method_name %||% ""]]$impl %||%
        def@impl$base %||%
        cli::cli_abort(c(
            "No variant {.val {method_name}} registered for model type {.val {model_type}}.",
            "i" = "Available variant{?s}: {.val {names(def@impl$variants)}}."
        ))

    all_args = utils::modifyList(
        .x@test_spec@args,
        .x@recalibrate_spec$args %||% list()
    )

    if (!is.null(impl@claim_parser) && !is.null(.x@claims)) {
        translated = impl@claim_parser(.x@claims, .x@processed)

        if (!inherits(translated, "claim_args")) {
            cli::cli_abort(
                "claim_parser must return a {.fn claim_args} object."
            )
        }

        all_args = utils::modifyList(all_args, unclass(translated))
    } else if (is.null(impl@claim_parser) && !is.null(.x@claims)) {
        cli::cli_abort(c(
            "No claim parser defined for variant {.val {method_name %||% \"default\"}}.",
            "i" = "Remove {.fn state_null} or use a supported variant."
        ))
    }

    out_raw = inject_and_run(
        impl = impl,
        processed = .x@processed,
        args = all_args
    )

    wrap_exec(
        out_raw,
        def = def,
        impl = impl,
        stat_cls = .x@test_spec@cls,
        stat_name = .x@test_spec@name,
        method_name = method_name,
        var_id = .x@var_id,
        processed = .x@processed,
        data_name = .x@data_name %||% ""
    )
}

S7::method(conclude, model_lazy) = function(.x, ...) {
    model_type = if (inherits(.x@var_id, "formula")) {
        "formula"
    } else {
        S7::S7_class(.x@var_id)@name
    }
    cls = .x@model_spec@cls
    lookup = if (
        identical(.x@model_spec@registry_version, stat_define_registry$.version)
    ) {
        .x@model_spec@lookup
    } else {
        build_lookup(.x@model_spec@defs, cls)
    }
    def = find_def(lookup, model_type = model_type)

    # method_name = .x@recalibrate_spec$method_name
    # cls = .x@model_spec@cls
    method_name = .x@recalibrate_spec$method_name
    key = variant_registry_key(cls, model_type)
    impl = def@impl$variants[[method_name %||% ""]] %||%
        variant_registry[[key]][[method_name %||% ""]]$impl %||%
        def@impl$base %||%
        cli::cli_abort(c(
            "No variant {.val {method_name}} registered for model type {.val {model_type}}.",
            "i" = "Available variant{?s}: {.val {names(def@impl$variants)}}."
        ))

    all_args = utils::modifyList(
        .x@model_spec@args,
        .x@recalibrate_spec$args %||% list()
    )

    # if (!is.null(impl@claim_parser) && !is.null(.x@claims)) {
    #     translated = impl@claim_parser(.x@claims, .x@processed)
    #
    #     if (!inherits(translated, "claim_args")) {
    #         cli::cli_abort(
    #             "claim_parser must return a {.fn claim_args} object."
    #         )
    #     }
    #
    #     all_args = utils::modifyList(all_args, unclass(translated))
    # } else if (is.null(impl@claim_parser) && !is.null(.x@claims)) {
    #     cli::cli_abort(c(
    #         "No claim parser defined for variant {.val {method_name %||% \"default\"}}.",
    #         "i" = "Remove {.fn state_null} or use a supported variant."
    #     ))
    # }

    out_raw = inject_and_run(
        impl = impl,
        processed = .x@processed,
        args = all_args
    )

    wrap_exec(
        out_raw,
        def = def,
        impl = impl,
        stat_cls = .x@model_spec@cls,
        stat_name = .x@model_spec@name,
        method_name = method_name,
        var_id = .x@var_id,
        processed = .x@processed,
        data_name = .x@data_name %||% ""
    )
}

resolve_impl = function(method_name, def, model_type, cls, global_variants) {
    if (is.null(method_name)) {
        return(def@impl$base)
    }

    global_entries = global_variants[[cls]] %||% list()
    global_match = Filter(
        function(e) identical(e$name, method_name),
        global_entries
    )

    def@impl$variants[[method_name]] %||%
        global_match[[1]]$impl %||%
        cli::cli_abort(c(
            "No variant {.val {method_name}} registered for model type {.val {model_type}}.",
            "i" = "Available variant{?s}: {.val {names(def@impl$variants)}}."
        ))
}

wrap_exec = function(
    out_raw,
    def,
    impl,
    stat_cls,
    stat_name,
    method_name,
    var_id,
    processed,
    data_name
) {
    cld_exec(
        data = out_raw,
        impl_cls = impl_cls_from_model(stat_cls, var_id),
        stat_cls = stat_cls,
        print_fn = impl@print,
        name = stat_name,
        cld_meta = list(
            var_id = var_id,
            processed = processed,
            stat_name = stat_name,
            method = method_name %||% "default",
            data_name = data_name %||% ""
        )
    )
}

cld_exec = S7::new_class(
    "cld_exec",
    parent = stat_infer_spec,
    properties = list(
        impl_cls = S7::new_property(class = S7::class_character),
        cld_meta = S7::new_property(class = S7::class_list)
    )
)

S7::method(print, cld_exec) = function(x, ...) {
    meta = x@cld_meta
    info = var_id_info(meta$var_id, meta$processed)

    cat("\n")
    cat(cli::rule(left = "Model", line = "="), "\n\n")
    cat("Variable Mapper :", info@model_type, "\n")
    cat("Args :", info@args, "\n")
    if (length(info@other_info) > 0L) {
        for (nm in names(info@other_info)) {
            cat("   ", nm, ":", info@other_info[[nm]], "\n")
        }
    }
    if (nzchar(meta$data_name)) {
        cat("Data     :", meta$data_name, "\n")
    }

    stat_label = if (identical(meta$method, "default")) {
        meta$stat_name
    } else {
        paste0(meta$stat_name, " \u00b7 ", meta$method)
    }
    cat("\n")
    cat(cli::rule(left = stat_label, line = "="), "\n\n")

    print_fn = x@print_fn
    if (!is.null(print_fn)) {
        print_fn(x, ...)
    } else {
        print(x@data)
    }

    invisible(x)
}
