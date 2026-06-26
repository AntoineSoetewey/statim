#' Define a layout supplied by a Variable Mapper
#'
#' `define_model()` captures a variable mapper `<var_id>` and optional data into a `def_var`
#' object that can be passed into [prepare_test()].
#'
#' @param .x A variable mapper `<var_id>` object from [x_by()], [rel()], [pairwise()], or a
#'   formula. It is also dispatched for a data frame class when using the data-first
#'   pipe style.
#' @param ... Currently unused.
#'
#' @details
#' Two dispatch methods are available depending on how `.x` is supplied:
#'
#' - **A "Variable Mapper" first**: `.x` is a Variable Mapper or formula. Accepts `data`, a
#'   data frame (defaults to `parent.frame()`).
#' - **DataFrame-first**: `.x` is a data frame. Accepts `to_analyze`, a variable mapper
#'   or formula, as the second argument.
#'
#' @return A `def_var` S3 object containing `var_id` and `processed`.
#'
#' @examples
#' # model-ID first
#' define_model(x_by(extra, group), sleep)
#'
#' # data-frame first (pipe-friendly)
#' sleep |> define_model(x_by(extra, group))
#'
#' @name layout-define-base
#' @export
define_model = S7::new_generic("define_model", ".x")

S7::method(define_model, S7::new_union(S7::class_formula, var_id)) = function(
    .x,
    data = parent.frame(),
    ...
) {
    def_var(var_id = .x, processed = model_processor(.x, data))
}

S7::method(define_model, S7::class_data.frame) = function(.x, to_analyze, ...) {
    def_var(var_id = to_analyze, processed = model_processor(to_analyze, .x))
}

def_var = S7::new_class(
    "def_var",
    properties = list(
        # var_id = S7::class_any,
        var_id = S7::new_property(
            class = S7::new_union(var_id, S7::class_formula)
        ),
        processed = S7::class_list
    )
)

S7::method(print, def_var) = function(x, ...) {
    info = var_id_info(x@var_id, x@processed)

    cat("\n")
    cat(cli::rule(left = "Model Definition", line = "-"), "\n\n")
    cat("Variable Mapper :", info@model_type, "\n")
    cat("Args :", info@args, "\n")

    if (!info@registered) {
        cat(
            "Note : Unregistered <var_id> subclass (must define a `var_id_info()` method.)\n"
        )
    } else {
        if (length(info@other_info)) {
            cat("Other info:\n")
            for (nm in names(info@other_info)) {
                cat("   ", nm, ":", info@other_info[[nm]], "\n")
            }
        }

        if (length(info@vars)) {
            cat("Variables :\n")
            for (v in info@vars) {
                cat("   ", v$name, ":", v$preview, "\n")
            }
        }
    }

    cat("\n")
    invisible(x)
}
