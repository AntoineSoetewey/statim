#' Automatically predict from a statistical result
#'
#' `auto_predict()` is the protocol generic for producing predictions from
#' result objects produced by `fn` in `baseline()` and `variant()`. It is
#' called automatically by `predict()` when the result stored in
#' `cld_exec@data` is a `class_stat_infer` subclass.
#'
#' Register a method on your output class to participate:
#'
#' ```r
#' S7::method(auto_predict, my_model_result) = function(x, new_data = NULL, ...) {
#'     tibble::tibble(.pred = ...)
#' }
#' ```
#'
#' A variant whose `fn` returns the same result class as `baseline`
#' inherits `auto_predict()` for free via S7's parent chain.
#'
#' @param x A `class_stat_infer` subclass object, typically `cld_exec@data`.
#' @param new_data A data frame. `NULL` defaults to the training data.
#' @param ... Currently unused. Passed to the dispatched method.
#'
#' @return A data frame with at minimum a `.pred` column.
#'
#' @seealso [predict()], [making_predict()], [method_predict()], [class_stat_infer]
#'
#' @export
auto_predict = S7::new_generic("auto_predict", "x", fun = function(x, ...) {
    if (!is_class_stat_infer(x)) {
        cli::cli_abort(c(
            "{.arg x} must inherit {.cls class_stat_infer}.",
            "x" = "Got {.cls {class(x)[[1]]}}."
        ))
    }
    S7::S7_dispatch()
})

S7::method(auto_predict, class_stat_infer) = function(x, ...) {
    cli::cli_abort(c(
        "No {.fn auto_predict} method for {.cls {class(x)[[1]]}}.",
        "i" = "Implement {.fn auto_predict} on your {.cls {class(x)[[1]]}} class."
    ))
}

S7::method(auto_predict, class_lm_object) = function(
    x,
    new_data = NULL,
    interval = c("none", "confidence", "prediction"),
    level = 0.95,
    ...
) {
    interval = rlang::arg_match(interval)

    if (interval == "prediction" && x@family != "gaussian") {
        cli::cli_abort(c(
            "{.arg interval = \"prediction\"} is only valid for Gaussian models.",
            "x" = "This model has family {.val {x@family}}."
        ))
    }

    x_new = predict_model_matrix(x, new_data)
    fit = as.vector(x_new %*% x@beta)
    out = tibble::tibble(.pred = fit)

    resp_name = as.character(attr(x@terms, "variables")[[
        attr(x@terms, "response") + 1L
    ]])
    truth = if (is.null(new_data)) {
        x@fitted + x@residuals
    } else if (resp_name %in% names(new_data)) {
        new_data[[resp_name]]
    } else {
        NULL
    }
    if (!is.null(truth)) {
        out = tibble::add_column(out, truth = truth, .before = ".pred")
    }

    if (interval != "none") {
        se_fit = delta_se(gradient = x_new, vcov = x@vcov)
        if (interval == "prediction") {
            se_fit = sqrt(se_fit^2 + x@dispersion)
        }
        crit = stats::qt(1 - (1 - level) / 2, df = x@df_residual)
        out$.pred_lower = fit - crit * se_fit
        out$.pred_upper = fit + crit * se_fit
    }

    out
}

predict_model_matrix = function(object, new_data) {
    if (is.null(new_data)) {
        n = length(object@fitted)
        ncols = length(object@beta)
        return(matrix(object@x_mat, nrow = n, ncol = ncols))
    }

    trms = stats::delete.response(object@terms)
    mf = stats::model.frame(
        trms,
        data = new_data,
        na.action = stats::na.pass,
        xlev = object@x_levels
    )
    stats::model.matrix(trms, data = mf)
}

#' @keywords internal
#' @noRd
delta_se = function(gradient, vcov) {
    sqrt(rowSums((gradient %*% vcov) * gradient))
}
