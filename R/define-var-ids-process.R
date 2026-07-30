#' Model evaluator
#'
#' A function for development use to extract the information in Variable Mappers.
#'
#' @param var_id The Variable Mappers to be extracted.
#' @param data Optional. Only passed when a certain data structure (normally it's data frame)
#'    is required.
#' @param ... Passed through S7 method compatibility.
#'
#' @details
#' Methods accept an optional `data` argument — a data frame, or `NULL`
#' to resolve variables from the calling environment.
#'
#' @returns A named list. The default method returns an empty list; each
#' registered method returns a list shaped for its `var_id` subclass (for
#' example, `x_data`/`group_data` for [x_by()], or `x`/`n` for [prop()]).
#'
#' @name model-processor
#' @export
model_processor = S7::new_generic(
    "model_processor",
    "var_id",
    fun = function(var_id, data = NULL, ...) S7::S7_dispatch()
)

S7::method(model_processor, var_id) = function(var_id, data = NULL, ...) {
    list()
}

S7::method(model_processor, S7::class_formula) = function(
    var_id,
    data = NULL,
    ...
) {
    vars = all.vars(var_id)
    data = if (rlang::is_null(data)) {
        vctrs::new_data_frame(rlang::set_names(
            lapply(vars, \(v) {
                rlang::eval_tidy(rlang::sym(v), env = rlang::f_env(var_id))
            }),
            vars
        ))
    } else {
        data
    }

    list(data = data, vars = vars, formula = var_id)
}

S7::method(model_processor, x_by) = function(var_id, data = NULL, ...) {
    proc = two_vars_extract(
        var_id@x,
        var_id@group,
        data = data,
        role2 = "group"
    )
    list(x_data = proc$x1_data, group_data = proc$x2_data)
}

S7::method(model_processor, rel) = function(var_id, data = NULL, ...) {
    proc = two_vars_extract(var_id@x, var_id@resp, data = data, role2 = "resp")
    list(x_data = proc$x1_data, resp_data = proc$x2_data)
}

S7::method(model_processor, pairwise) = function(var_id, data = NULL, ...) {
    pairwise_data_extract(var_id, data)
}

S7::method(model_processor, prop) = function(var_id, data = NULL, ...) {
    list(x = var_id@x, n = var_id@n)
}

S7::method(model_processor, on) = function(var_id, data = NULL, ...) {
    multiple_vars_extract(var_id, data)
}
