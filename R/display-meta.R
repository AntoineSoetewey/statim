#' Display individual results
#'
#' `display()` prints the first `n` concluded models from an abstract S7
#' class, e.g. `multi_exec`, in full. Useful when [conclude()] has been called on a
#' [write_models()] pipeline and the default compressed print is not enough.
#'
#' @param x An object yield by [conclude()].
#' @param n A positive integer. The number of models' output to display. Defaults
#'   to `3`.
#' @param ... Currently unused.
#'
#' @return `x`, invisibly.
#'
#' @seealso [write_models()], [conclude()], [tidy()]
#'
#' @examples
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
#'     display(2)
#'
#' @export
display = S7::new_generic(
    "display",
    "x",
    fun = function(x, n = 3L, ...) S7::S7_dispatch()
)

S7::method(display, multi_exec) = function(x, n = 3L, ...) {
    n = min(n, length(x@results))
    for (i in seq_len(n)) {
        cat(sprintf("\n%d. %s\n", i, x@labels[[i]]))
        print(x@results[[i]])
    }
    invisible(x)
}
