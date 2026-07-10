#' Effect size for a concluded statistical result
#'
#' `gauge()` reports the standardized magnitude of an effect — Cohen's d,
#' partial eta-squared, odds ratio, and similar quantities — as distinct
#' from `tidy()`'s raw estimates and the p-value's significance verdict.
#'
#' @section Dispatch:
#' Same two paths as `tidy()`/`glance()`/`predict()`:
#'
#' **Path 1: `auto_gauge()` (preferred).**
#' Called directly when `cld_exec@data` is a `class_stat_infer` subclass.
#'
#' **Path 2: `making_gauge()` registry (escape hatch).**
#' Used when a variant's `fn` intentionally returns a non-`class_stat_infer`
#' object.
#'
#' @param object A `cld_exec` object produced by `conclude()`.
#' @param ... Passed to the dispatched method.
#'
#' @return A tibble with `metric` and `value` columns, one row per
#'   effect-size quantity reported by the underlying result class.
#'
#' @seealso [conclude()], [auto_gauge()], [making_gauge()]
#'
#' @export
gauge = S7::new_generic("gauge", "object")

S7::method(gauge, cld_exec) = function(object, ...) {
    if (is_class_stat_infer(object@data)) {
        return(auto_gauge(object@data, ...))
    }

    key = gauge_registry_key(object@impl_cls)
    mg = register_gauge[[key]]
    impl_cls = object@impl_cls

    if (is.null(mg)) {
        cli::cli_abort(c(
            "No gauge method found for {.val {impl_cls}}.",
            "i" = "Either return a {.cls class_stat_infer} subclass from {.fn fn},",
            "i" = "or register a gauge method via {.fn making_gauge}."
        ))
    }

    method_nm = object@cld_meta$method
    gauge_fn = if (identical(method_nm, "default")) {
        mg@default
    } else if (!is.null(mg@variants[[method_nm]])) {
        mg@variants[[method_nm]]
    } else {
        cli::cli_abort(c(
            "No gauge entry for variant {.val {method_nm}} in {.val {impl_cls}}.",
            "i" = "Add {.code {method_nm} =} to {.fn method_gauge}, or return a",
            "i" = "{.cls class_stat_infer} subclass from {.fn fn} to use {.fn auto_gauge}."
        ))
    }

    if (is.null(gauge_fn)) {
        cli::cli_abort(c(
            "No {.arg default} gauge function registered for {.val {impl_cls}}.",
            "i" = "Supply {.code default =} in {.fn method_gauge}."
        ))
    }

    gauge_fn(object, ...)
}

#' Declare a gauge method for a stat and model type
#'
#' `making_gauge()` is the escape hatch for registering effect-size
#' methods when a variant's `fn` intentionally returns a non-
#' `class_stat_infer` object. When `fn` returns a `class_stat_infer`
#' subclass, implement `auto_gauge()` on the result class instead — no
#' registration needed.
#'
#' @param obj A stat function built with [HTEST_FN()] or [MODEL_FN()]
#'   (e.g. `T_TEST`, `LINEAR_REG`).
#' @param model_type An S7 variable mapper `<var_id>` class, or
#'   `S7::class_formula`.
#'
#' @return A `making_gauge_call` object, consumed by `%<-%`.
#'
#' @seealso [auto_gauge()], [method_gauge()], [class_stat_infer]
#'
#' @examples
#' # Only needed when fn returns a non-class_stat_infer object.
#' # Prefer implementing auto_gauge() on your result class instead.
#' making_gauge(T_TEST, x_by) %<-% method_gauge(
#'     default = function(.x, ...) { ... }
#' )
#'
#' @export
making_gauge = function(obj, model_type) {
    structure(
        list(obj = obj, model_type = model_type),
        class = "making_gauge_call"
    )
}

#' @keywords internal
making_gauge_register = function(lhs, rhs) {
    obj = lhs$obj
    model_type = lhs$model_type

    stat_cls = attr(obj, "cls") %||%
        cli::cli_abort(
            "{.arg obj} must be a function built with {.fn HTEST_FN} or {.fn MODEL_FN}."
        )
    is_var_id_class = inherits(model_type, "S7_class") &&
        identical(model_type@parent, var_id)
    is_formula_class = identical(model_type, S7::class_formula)
    if (!is_var_id_class && !is_formula_class) {
        cli::cli_abort(
            "{.arg model_type} must be a class inheriting from {.cls var_id}, or {.code S7::class_formula}."
        )
    }
    if (!S7::S7_inherits(rhs, method_gauge)) {
        cli::cli_abort(
            "Right-hand side of {.code %<-%} must be a {.cls method_gauge} object."
        )
    }

    key = gauge_registry_key(paste0(stat_cls, "_", model_type_name(model_type)))
    existing = register_gauge[[key]]
    if (is.null(existing)) {
        register_gauge[[key]] = rhs
    } else {
        merged_default = rhs@default %||% existing@default
        merged_variants = utils::modifyList(existing@variants, rhs@variants)
        register_gauge[[key]] = do.call(
            method_gauge,
            c(list(merged_default), merged_variants)
        )
    }
    invisible(NULL)
}

#' Declare gauge methods for a stat result
#'
#' `method_gauge()` is the companion to `making_gauge()`. It collects
#' effect-size functions for the base implementation and named variants,
#' used only when `fn` returns a non-`class_stat_infer` object.
#'
#' @param default A function with signature `function(.x, ...)`, returning
#'   a tibble with `metric` and `value` columns. Required.
#' @param ... Named functions, one per variant. Names must match variant
#'   names registered in `agendas()`. Omitted variants fall back to
#'   `default` automatically.
#'
#' @return A `method_gauge` S7 object.
#'
#' @seealso [making_gauge()], [auto_gauge()], [class_stat_infer]
#'
#' @export
method_gauge = S7::new_class(
    "method_gauge",
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

register_gauge = new.env(parent = emptyenv())
gauge_registry_key = function(impl_cls) impl_cls
