expanded_model = S7::new_class(
    "expanded_model",
    properties = list(
        models = S7::new_property(class = S7::class_list),
        labels = S7::new_property(class = S7::class_character)
    )
)

multi_lazy = S7::new_class(
    "multi_lazy",
    properties = list(
        models = S7::class_list,
        labels = S7::new_property(
            class = S7::class_character,
            default = character(0)
        ),
        args = S7::new_property(class = S7::class_list, default = list())
    )
)

multi_exec = S7::new_class(
    "multi_exec",
    properties = list(
        results = S7::new_property(class = S7::class_list),
        labels = S7::new_property(
            class = S7::class_character,
            default = character(0)
        ),
        stat_name = S7::new_property(class = S7::class_character, default = "")
    )
)

#' Write multiple model definitions from a data frame
#'
#' `write_models()` evaluates named model expressions sequentially against
#' `.data`, so each name is available to subsequent expressions via
#' [stats::update()]. Accepts any valid variable mapper `<var_id>`: `<formulas>`, [rel()],
#' [x_by()], or any registered `var_id` type.
#'
#' Sits between a data frame and [prepare_model()] or [prepare_test()]
#' in the pipeline.
#'
#' @param .data A data frame.
#' @param ... Named model expressions. Each must evaluate to a formula or
#'   a `var_id` object. Names are used as row labels in [anova()] output
#'   and as the `model` column in [tidy()].
#'
#' @return An `expanded_model` object.
#'
#' @seealso [prepare_model()], [prepare_test()], [anova()], [conclude()],
#'   [display()]
#'
#' @examples
#' # explicit formulas
#' LifeCycleSavings |>
#'     write_models(
#'         f1 = sr ~ 1,
#'         f2 = sr ~ pop15,
#'         f3 = sr ~ pop15 + pop75,
#'         f4 = sr ~ pop15 + pop75 + dpi,
#'         f5 = sr ~ pop15 + pop75 + dpi + ddpi
#'     ) |>
#'     prepare_model(LINEAR_REG) |>
#'     anova()
#'
#' # update() chain (formulas only)
#' LifeCycleSavings |>
#'     write_models(
#'         f1 = sr ~ 1,
#'         f2 = update(f1, ~. + pop15),
#'         f3 = update(f2, ~. + pop75),
#'         f4 = update(f3, ~. + dpi),
#'         f5 = update(f4, ~. + ddpi)
#'     ) |>
#'     prepare_model(LINEAR_REG) |>
#'     anova()
#'
#' # conclude() -> returns a multi_exec
#' LifeCycleSavings |>
#'     write_models(
#'         f1 = sr ~ 1,
#'         f2 = sr ~ pop15,
#'         f3 = sr ~ pop15 + pop75
#'     ) |>
#'     prepare_model(LINEAR_REG) |>
#'     conclude()
#'
#' \dontrun{
#' # display() -> show up to n models in full
#' LifeCycleSavings |>
#'     write_models(
#'         f1 = sr ~ 1,
#'         f2 = sr ~ pop15,
#'         f3 = sr ~ pop15 + pop75,
#'         f4 = sr ~ pop15 + pop75 + dpi,
#'         f5 = sr ~ pop15 + pop75 + dpi + ddpi
#'     ) |>
#'     prepare_model(LINEAR_REG) |>
#'     conclude() |>
#'     display(5)
#' }
#'
#' # via rel()
#' mtcars |>
#'     define_model(rel(wt, mpg)) |>
#'     prepare_model(LINEAR_REG) |>
#'     anova()
#' mtcars |>
#'     write_models(
#'         m1 = rel(wt, mpg),
#'         m2 = rel(hp, mpg)
#'     ) |>
#'     prepare_model(LINEAR_REG) |>
#'     anova()
#'
#' # mixed var_id types in a single write_models() call
#' suppressWarnings({
#'     mtcars |>
#'         write_models(
#'             null = mpg ~ 1,
#'             m1 = rel(wt, mpg),
#'             m2 = rel(hp, mpg)
#'         ) |>
#'         prepare_model(LINEAR_REG) |>
#'         conclude()
#' })
#'
#' # via prepare_test()
#' mtcars |>
#'     write_models(
#'         by_am = x_by(mpg, am),
#'         by_vs = x_by(mpg, vs)
#'     ) |>
#'     prepare_test(T_TEST) |>
#'     conclude()
#'
#' @export
write_models = S7::new_generic("write_models", ".data")

S7::method(write_models, S7::class_data.frame) = function(.data, ...) {
    quos = rlang::enquos(...)
    nms = names(quos)

    if (is.null(nms) || any(!nzchar(nms))) {
        cli::cli_abort("All arguments to {.fn write_models} must be named.")
    }

    env = rlang::new_data_mask(rlang::new_environment(
        parent = rlang::caller_env()
    ))

    models = vector("list", length(quos))
    names(models) = nms

    for (i in seq_along(quos)) {
        val = rlang::eval_tidy(quos[[i]], data = env)
        env[[nms[[i]]]] = val
        models[[i]] = def_var(
            var_id = val,
            processed = model_processor(val, .data)
        )
    }

    expanded_model(models = models, labels = nms)
}

S7::method(print, expanded_model) = function(x, ...) {
    cat("\n")
    cat(cli::rule(left = "Models", line = "-"), "\n\n")
    for (i in seq_along(x@models)) {
        m = x@models[[i]]
        lbl = x@labels[[i]]
        cat(sprintf("  %s : %s\n", lbl, var_id_info(m@var_id)@args))
    }
    cat("\n")
    invisible(x)
}

S7::method(print, multi_exec) = function(x, ...) {
    n_models = length(x@results)
    header = sprintf(
        "%d model%s \u00b7 %s",
        n_models,
        if (n_models == 1L) "" else "s",
        x@stat_name
    )

    cat("\n")
    cat(cli::rule(left = header), "\n\n")
    for (i in seq_along(x@results)) {
        cat(sprintf("%s : <cld_exec>\n", x@labels[[i]]))
    }
    cat("\n")
    cli::cat_line(cli::col_silver(
        "Use display() to inspect individual results."
    ))
    cat("\n")
    invisible(x)
}

S7::method(conclude, multi_lazy) = function(.x, ...) {
    results = lapply(.x@models, conclude)
    stat_name = if (length(.x@models) > 0L) {
        results[[1L]]@cld_meta$stat_name %||% ""
    } else {
        ""
    }
    multi_exec(
        results = rlang::set_names(results, .x@labels),
        labels = .x@labels,
        stat_name = stat_name
    )
}

S7::method(update, multi_lazy) = function(object, ...) {
    dots = list(...)
    object@models = lapply(object@models, function(m) {
        if (!is.null(m@recalibrate_spec)) {
            m@recalibrate_spec$args = utils::modifyList(
                m@recalibrate_spec$args,
                dots
            )
        } else {
            m@model_spec@args = utils::modifyList(m@model_spec@args, dots)
        }
        m
    })
    object
}

S7::method(tidy, multi_exec) = function(.x, ...) {
    tidied = lapply(.x@results, function(r) tidy(r, ...))
    tibble::tibble(model = .x@labels, outs = tidied)
}

S7::method(prepare_model, list(expanded_model, S7::class_function)) = function(
    .x,
    .model_fn,
    ...
) {
    spec = as_model_spec(.model_fn)
    models = lapply(.x@models, function(dm) {
        model_lazy(
            var_id = dm@var_id,
            processed = dm@processed,
            model_spec = spec
        )
    })
    multi_lazy(models = models, labels = .x@labels, args = list())
}

S7::method(prepare_test, list(expanded_model, S7::class_function)) = function(
    .x,
    .test,
    ...
) {
    spec = as_test_spec(.test)
    models = lapply(.x@models, function(dm) {
        test_lazy(
            var_id = dm@var_id,
            processed = dm@processed,
            test_spec = spec
        )
    })
    multi_lazy(models = models, labels = .x@labels, args = list())
}

S7::method(prepare, list(expanded_model, S7::class_function)) = function(
    .x,
    .fn,
    ...
) {
    spec = tryCatch(.fn(.var_id = NULL), error = function(e) {
        cli::cli_abort(
            "{.arg .fn} must be a function built with {.fn STAT_CONSTRUCTOR}.",
            parent = e
        )
    })

    builder = if (is_test_spec(spec)) {
        function(dm) {
            test_lazy(
                var_id = dm@var_id,
                processed = dm@processed,
                test_spec = spec
            )
        }
    } else if (is_model_spec(spec)) {
        function(dm) {
            model_lazy(
                var_id = dm@var_id,
                processed = dm@processed,
                model_spec = spec
            )
        }
    } else {
        cli::cli_abort(
            "{.arg .fn} must return a {.cls test_spec} or {.cls model_spec}."
        )
    }

    models = lapply(.x@models, builder)
    multi_lazy(models = models, labels = .x@labels, args = list())
}

S7::method(via, list(multi_lazy, S7::class_character)) = function(
    .x,
    .method,
    ...
) {
    .x@models = lapply(.x@models, function(m) via(m, .method, ...))
    .x
}

S7::method(print, multi_lazy) = function(x, ...) {
    cat("\n")
    cat(cli::rule(left = "Models", line = "-"), "\n\n")
    for (i in seq_along(x@models)) {
        m = x@models[[i]]
        lbl = x@labels[[i]]
        cat(sprintf("  %s : %s\n", lbl, var_id_info(m@var_id)@args))
    }
    cat("\n")
    invisible(x)
}
