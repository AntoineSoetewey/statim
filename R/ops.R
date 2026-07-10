#' Apply a method_tidy to a making_tidy target
#'
#' `%<-%` registers a [method_tidy()] into the tidy registry. The
#' left-hand side must be a [making_tidy()] call.
#'
#' @param lhs A `making_tidy_call` object from [making_tidy()].
#' @param rhs A [method_tidy()] object.
#'
#' @return `NULL` invisibly, called for its side effects.
#'
#' @examples
#' making_tidy(T_TEST, x_by) %<-% method_tidy(
#'     default = function(.x, ...) { ... },
#'     boot = function(.x, ...) { ... }
#' )
#'
#' @name modifying-assignment
#' @export
`%<-%` = function(lhs, rhs) {
    if (inherits(lhs, "add_variant_call")) {
        add_variant_register(lhs, rhs)
    } else if (inherits(lhs, "making_tidy_call")) {
        making_tidy_register(lhs, rhs)
    } else if (inherits(lhs, "making_glance_call")) {
        making_glance_register(lhs, rhs)
    } else if (inherits(lhs, "making_predict_call")) {
        making_predict_register(lhs, rhs)
    } else if (inherits(lhs, "making_gauge_call")) {
        making_gauge_register(lhs, rhs)
    } else {
        cli::cli_abort(
            "Left-hand side of {.code %<-%} must be an {.fn add_variant}, {.fn making_tidy}, {.fn making_glance}, {.fn making_predict}, or {.fn making_gauge} call."
        )
    }
}
