infer_context = function(
    processed,
    args,
    extractors,
    claims = NULL,
    method_args = NULL
) {
    out = list(
        processed = processed,
        args = args,
        extractors = extractors,
        claims = claims,
        method_args = method_args %||% list()
    )
    class(out) = "infer_context"
    out
}

#' Access data and arguments inside a test implementation
#'
#' These functions provide access to variables, arguments, and claims
#' inside the `run` function of a `test_define()` object. The `self`
#' parameter in `run` is an `infer_context` object — pass it as `x`
#' to these functions.
#'
#' @param x An `infer_context` object passed as `self` inside `run`.
#' @param role A string naming the variable role declared in `vars`.
#'   E.g. `"x"`, `"group"`.
#' @param name A string naming the argument or claim.
#' @param default A fallback value if the argument was not supplied.
#'
#' @return
#' - `ic_pull()` — a vector
#' - `ic_name()` — a string
#' - `ic_arg()` — the argument value or `default`
#' - `ic_method_arg()` — the method argument value or `default`
#' - `ic_claim()` — a `ClaimDef` object or `NULL`
#'
#' @seealso `test_define()`, `method_spec()`, [via()], [conclude()]
#'
#' @examples
#' \dontrun{
#' test_new_def = test_define(
#'     model_type = "x_by",
#'     impl_class = "test_new_def_in_two",
#'     vars = list(
#'         x = function(p) p$x_data[[1]],
#'         group = function(p) p$group_data[[1]]
#'     ),
#'     run = function(self) {
#'         grp = as.character(ic_pull(self, "group"))
#'         resp = ic_pull(self, "x")
#'         n = ic_method_arg(self, "n")
#'         ci = ic_arg(self, ".ci", 0.95)
#'     }
#' )
#' }
#'
#' @name infer-context-accessors
NULL

#' @rdname infer-context-accessors
#' @export
ic_pull = function(x, role) {
    extractor = x$extractors[[role]]
    if (is.null(extractor))
        cli::cli_abort("No extractor for role {.val {role}}.")
    extractor(x$processed)
}

#' @rdname infer-context-accessors
#' @export
ic_name = function(x, role) {
    val = ic_pull(x, role)
    if (is.data.frame(val)) names(val)[[1]] else role
}

#' @rdname infer-context-accessors
#' @export
ic_arg = function(x, name, default = NULL) {
    x$args[[name]] %||% default
}

#' @rdname infer-context-accessors
#' @export
ic_method_arg = function(x, name, default = NULL) {
    x$method_args[[name]] %||% default
}

#' @rdname infer-context-accessors
#' @export
ic_claim = function(x, name) {
    x$claims[[name]]
}
