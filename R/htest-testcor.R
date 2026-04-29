#' Correlation Test
#'
#' `CORTEST()` performs a correlation test for one-to-one or many-to-one
#' variable relationships.
#'
#' @param .model A model ID from `rel()`. When supplied, the test executes
#'   immediately. When `NULL` (default), returns a `test_spec` for use in
#'   the pipeline via [prepare_test()].
#' @param .data A data frame. Only used on the standalone path.
#' @param ... Additional arguments passed to the implementation:
#'   `.cor_type`, `.alt`, `.ci` for the classical path.
#'
#' @return An `htest_spec` object (standalone or eager), or a `test_spec`
#'   object (pipeline).
#'
#' @section Supported model IDs:
#' - `rel()` — many-to-one correlation test
#'
#' @examples
#' # eager
#' CORTEST(rel(speed, dist), cars)
#'
#' # pipeline
#' cars |>
#'     define_model(rel(speed, dist)) |>
#'     prepare_test(CORTEST) |>
#'     conclude()
#'
#' @export
CORTEST = HTEST_FN(
    cls = "cortest",
    defs = list(
        cor_test_rel
    ),
    .name = "Correlation Test"
)
