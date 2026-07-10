#' Predict from a concluded statistical result
#'
#' `predict()` estimates the response for new or existing rows from a
#' `cld_exec` object produced by `conclude()`.
#'
#' @section Dispatch:
#' Dispatches on both `object` and `new_data`, so passing anything other
#' than a data frame (or omitting `new_data` entirely) fails at the call
#' boundary rather than inside the implementation. `tibble` and
#' `data.table` both satisfy the `data.frame` dispatch, since both inherit
#' from it.
#'
#' Within a given `new_data` type, two paths are tried in order:
#'
#' **Path 1: `auto_predict()` (preferred).**
#' Called directly when `cld_exec@data` is a `class_stat_infer` subclass.
#'
#' **Path 2: `making_predict()` registry (escape hatch).**
#' Used when a variant's `fn` intentionally returns a non-`class_stat_infer`
#' object.
#'
#' @param object A `cld_exec` object produced by `conclude()`.
#' @param new_data A data frame (or subclass, e.g. `tibble`, `data.table`).
#'   Defaults to the training data when omitted.
#' @param ... Passed to the dispatched method.
#'
#' @return A data frame (specifically a `tibble`) with `.pred`, `truth`
#'   when a response is available, and `.pred_lower`/`.pred_upper` when
#'   an interval was requested. Always inherits `data.frame`, regardless
#'   of which method produced it — enforced at the dispatch boundary, not
#'   left to each method to honor.
#'
#' @seealso [conclude()], [auto_predict()], [making_predict()]
#'
#' @export
predict = S7::new_external_generic("stats", "predict", c("object", "new_data"))

S7::method(predict, list(cld_exec, S7::class_missing)) = function(
    object,
    new_data,
    ...
) {
    dispatch_predict(object, new_data = NULL, ...)
}

S7::method(predict, list(cld_exec, S7::class_data.frame)) = function(
    object,
    new_data,
    ...
) {
    if (inherits(new_data, "data.table")) {
        new_data = as.data.frame(new_data)
    }
    dispatch_predict(object, new_data = new_data, ...)
}

#' @keywords internal
#' @noRd
dispatch_predict = function(object, new_data, ...) {
    out = if (is_class_stat_infer(object@data)) {
        auto_predict(object@data, new_data = new_data, ...)
    } else {
        key = predict_registry_key(object@impl_cls)
        mp = register_predict[[key]]
        impl_cls = object@impl_cls

        if (is.null(mp)) {
            cli::cli_abort(c(
                "No predict method found for {.val {impl_cls}}.",
                "i" = "Either return a {.cls class_stat_infer} subclass from {.fn fn},",
                "i" = "or register a predict method via {.fn making_predict}."
            ))
        }

        method_nm = object@cld_meta$method
        predict_fn = if (identical(method_nm, "default")) {
            mp@default
        } else if (!is.null(mp@variants[[method_nm]])) {
            mp@variants[[method_nm]]
        } else {
            cli::cli_abort(c(
                "No predict entry for variant {.val {method_nm}} in {.val {impl_cls}}.",
                "i" = "Add {.code {method_nm} =} to {.fn method_predict}, or return a",
                "i" = "{.cls class_stat_infer} subclass from {.fn fn} to use {.fn auto_predict}."
            ))
        }

        if (is.null(predict_fn)) {
            cli::cli_abort(c(
                "No {.arg default} predict function registered for {.val {impl_cls}}.",
                "i" = "Supply {.code default =} in {.fn method_predict}."
            ))
        }

        predict_fn(object, new_data = new_data, ...)
    }

    if (!inherits(out, "data.frame")) {
        cli::cli_abort(c(
            "{.fn predict} methods must return a {.cls data.frame} (a {.cls tibble} or {.cls data.table} included).",
            "x" = "Got {.obj_type_friendly {out}}."
        ))
    }

    out
}

#' Declare a predict method for a stat and model type
#'
#' `making_predict()` is the escape hatch for registering predict methods
#' when a variant's `fn` intentionally returns a non-`class_stat_infer`
#' object. When `fn` returns a `class_stat_infer` subclass, implement
#' `auto_predict()` on the result class instead — no registration needed.
#'
#' @param obj A stat function built with [MODEL_FN()] (e.g. `LINEAR_REG`).
#' @param model_type An S7 variable mapper `<var_id>` class, or
#'   `S7::class_formula`.
#'
#' @return A `making_predict_call` object, consumed by `%<-%`.
#'
#' @seealso [auto_predict()], [method_predict()], [class_stat_infer]
#'
#' @examples
#' # Only needed when fn returns a non-class_stat_infer object.
#' # Prefer implementing auto_predict() on your result class instead.
#' making_predict(LINEAR_REG, S7::class_formula) %<-% method_predict(
#'     default = function(.x, new_data = NULL, ...) { ... }
#' )
#'
#' @export
making_predict = function(obj, model_type) {
    structure(
        list(obj = obj, model_type = model_type),
        class = "making_predict_call"
    )
}

#' @keywords internal
making_predict_register = function(lhs, rhs) {
    obj = lhs$obj
    model_type = lhs$model_type

    stat_cls = attr(obj, "cls") %||%
        cli::cli_abort(
            "{.arg obj} must be a function built with {.fn MODEL_FN}."
        )
    is_var_id_class = inherits(model_type, "S7_class") &&
        identical(model_type@parent, var_id)
    is_formula_class = identical(model_type, S7::class_formula)
    if (!is_var_id_class && !is_formula_class) {
        cli::cli_abort(
            "{.arg model_type} must be a class inheriting from {.cls var_id}, or {.code S7::class_formula}."
        )
    }
    if (!S7::S7_inherits(rhs, method_predict)) {
        cli::cli_abort(
            "Right-hand side of {.code %<-%} must be a {.cls method_predict} object."
        )
    }

    key = predict_registry_key(paste0(
        stat_cls,
        "_",
        model_type_name(model_type)
    ))
    existing = register_predict[[key]]
    if (is.null(existing)) {
        register_predict[[key]] = rhs
    } else {
        merged_default = rhs@default %||% existing@default
        merged_variants = utils::modifyList(existing@variants, rhs@variants)
        register_predict[[key]] = do.call(
            method_predict,
            c(list(merged_default), merged_variants)
        )
    }
    invisible(NULL)
}

#' Declare predict methods for a stat result
#'
#' `method_predict()` is the companion to `making_predict()`. It collects
#' predict functions for the base implementation and named variants, used
#' only when `fn` returns a non-`class_stat_infer` object.
#'
#' @param default A function with signature `function(.x, new_data = NULL, ...)`.
#'   Required.
#' @param ... Named functions, one per variant. Names must match variant
#'   names registered in `agendas()`. Omitted variants fall back to
#'   `default` automatically.
#'
#' @return A `method_predict` S7 object.
#'
#' @seealso [making_predict()], [auto_predict()], [class_stat_infer]
#'
#' @export
method_predict = S7::new_class(
    "method_predict",
    properties = list(
        default = S7::new_property(default = NULL),
        variants = S7::new_property(class = S7::class_list, default = list())
    ),
    constructor = function(default = NULL, ...) {
        variants = list(...)
        if (!is.null(default) && !is.function(default)) {
            cli::cli_abort("{.arg default} must be a function or {.val NULL}.")
        }
        bad = !vapply(variants, is.function, logical(1))
        if (any(bad)) {
            cli::cli_abort(
                "All variant entries must be functions. Non-function: {.arg {names(variants)[bad]}}."
            )
        }
        S7::new_object(S7::S7_object(), default = default, variants = variants)
    }
)

register_predict = new.env(parent = emptyenv())
predict_registry_key = function(impl_cls) impl_cls
