#' Build a claim parser from named resolver functions
#'
#' `map_claim()` produces a parser function by mapping impl `fn` formal
#' names to resolver functions. Each resolver receives `(claim, processed)`
#' and returns the value for its argument. Resolvers that only need `claim`
#' can simply ignore `processed`.
#'
#' Pass the result as `claim_parser` to [baseline()] or [variant()]. A
#' variant without a `claim_parser` simply does not support [state_null()];
#' `conclude()` raises an error if a claim was stated but the active variant
#' has none.
#'
#' @param ... Named resolver functions. Names must match formals of the
#'   impl's `fn`. Each resolver has signature `function(claim, processed)`.
#'
#' @return A function of class `"map_claim"` with signature
#'   `function(claim, processed)`.
#'
#' @export
map_claim = function(...) {
    resolvers = list(...)
    if (is.null(names(resolvers)) || any(!nzchar(names(resolvers)))) {
        cli::cli_abort("All arguments to {.fn map_claim} must be named.")
    }
    invalid = !vapply(resolvers, is.function, logical(1))
    if (any(invalid)) {
        cli::cli_abort(
            "All arguments to {.fn map_claim} must be functions: {.val {names(resolvers)[invalid]}}."
        )
    }
    fn = function(claim, processed) {
        args = lapply(resolvers, function(r) r(claim, processed))
        do.call(claim_args, args)
    }
    structure(fn, class = c("map_claim", "function"))
}
