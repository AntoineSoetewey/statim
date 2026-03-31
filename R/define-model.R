#' Model define constructor
#'
#' @name model-define-base
#' @export
define_model = function(.x, ...) {
    if (inherits(.x, "formula")) {
        class(.x) = c(class(.x), "model_id")
    }
    UseMethod("define_model")
}

#' @rdname model-define-base
#' @export
define_model.model_id = function(.x, data = parent.frame(), ...) {
    metad = model_processor(.x, data)

    model_id = if (inherits(.x, "formula")) {
        out = list(formula = metad$formula)
        class(out) = "formula"
        out
    } else {
        .x
    }

    out = list(
        model_id = model_id,
        # options = vctrs::vec_c(...),
        processed = metad
    )
    class(out) = "def_model"
    out
}

#' @rdname model-define-base
#' @export
define_model.data.frame = function(.x, to_analyze, ...) {
    metad = model_processor(to_analyze, .x)

    model_id = if (inherits(to_analyze, "formula")) {
        out = list(formula = metad$formula)
        class(out) = "formula"
        out
    } else {
        to_analyze
    }

    out = list(
        model_id = model_id,
        # options = vctrs::vec_c(...),
        processed = metad
    )
    class(out) = "def_model"
    out
}
