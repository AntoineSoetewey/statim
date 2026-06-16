#' Extract a scalar hypothesis value from a null claim
#'
#' Rearranges a hypothesis of the form `c * PARAM + d == scalar` by moving
#' all numeric terms to the right-hand side. Unlike [claim_contrast_coefs()],
#' this does not require or validate a linear combination — it is suitable for
#' single-parameter claims involving any [param_obj] subclass (e.g. [MU()],
#' [PI()], [RHO()]).
#'
#' @param claim A `null_claim` object.
#' @param solve_coef Logical. If `TRUE`, divides the scalar by the parameter's
#'   coefficient `c`, fully solving for the parameter value `(scalar - d) / c`.
#'   Errors if `c == 0`. If `FALSE`, returns `scalar - d` only, leaving `c`
#'   on the parameter side. Default `FALSE`.
#'
#' @return A list with fields:
#' \describe{
#'   \item{`coefs`}{Named numeric vector of length 1: the coefficient `c` on
#'     the parameter term.}
#'   \item{`scalar`}{Numeric. The resolved scalar value after rearrangement.}
#'   \item{`op`}{Character. The (possibly flipped) relational operator.}
#' }
#'
#' @seealso [claim_contrast_coefs()]
#'
#' @export
claim_scalar = function(claim, solve_coef = FALSE) {
    lhs_terms = collect_terms(claim@lhs, sign = 1L)
    rhs_terms = collect_terms(claim@rhs, sign = -1L)
    all_terms = c(lhs_terms, rhs_terms)

    param_terms = Filter(function(t) t$kind == "param", all_terms)
    scalar_terms = Filter(function(t) t$kind == "scalar", all_terms)

    if (length(param_terms) == 0L) {
        cli::cli_abort(c(
            "No population parameter found in hypothesis.",
            "i" = "At least one side must contain a parameter like {.fn MU}, {.fn PI}, {.fn RHO}, etc."
        ))
    }

    if (length(param_terms) > 1L) {
        cli::cli_abort(c(
            "{.fn claim_scalar} expects a single parameter term.",
            "i" = "Found {length(param_terms)} parameter term{?s}.",
            "i" = "Use {.fn claim_contrast_coefs} for multi-parameter hypotheses."
        ))
    }

    coef_val = param_terms[[1]]$coef
    node = param_terms[[1]]$node
    nm = extract_param_name(node)

    scalar_val = -Reduce("+", lapply(scalar_terms, `[[`, "value"), 0)

    if (solve_coef) {
        if (coef_val == 0) {
            cli::cli_abort(c(
                "Cannot solve for parameter: coefficient is zero.",
                "i" = "A zero coefficient makes the hypothesis degenerate.",
                "i" = "Check the hypothesis expression for {.code {nm}}."
            ))
        }
        scalar_val = scalar_val / coef_val
        coef_val = 1
    }

    op = claim@op
    lhs_has_only_scalars = !any(vapply(lhs_terms, function(t) t$kind == "param", logical(1)))
    if (lhs_has_only_scalars && length(lhs_terms) > 0L) {
        op = unname(FLIP_OP[op])
    }

    coefs = c(coef_val)
    names(coefs) = nm

    list(coefs = coefs, scalar = scalar_val, op = op)
}

#' Extract contrast coefficients from a null claim
#'
#' Decomposes the hypothesis into a named numeric vector of coefficients,
#' one per `param_obj` term, plus the hypothesized scalar value and operator.
#'
#' @param claim A `null_claim` object.
#'
#' @return A list with fields `coefs`, `scalar`, and `op`.
#'
#' @export
claim_contrast_coefs = function(claim, filter = NULL) {
    assert_linear(claim@lhs, "claim_contrast_coefs")
    assert_linear(claim@rhs, "claim_contrast_coefs")

    lhs_terms = collect_terms(claim@lhs, sign = 1L)
    rhs_terms = collect_terms(claim@rhs, sign = -1L)
    all_terms = c(lhs_terms, rhs_terms)

    param_terms = Filter(function(t) t$kind == "param", all_terms)
    scalar_terms = Filter(function(t) t$kind == "scalar", all_terms)

    if (length(param_terms) == 0L) {
        cli::cli_abort(c(
            "No population parameter found in hypothesis.",
            "i" = "At least one side must contain a parameter like {.fn MU}, {.fn PI}, etc."
        ))
    }

    bad = Filter(function(t) {
        node = t$node
        cls = S7::S7_class(node)
        slot_name = if (is.null(filter)) {
            fmls = names(formals(cls@constructor))
            matched = intersect(fmls, S7::prop_names(node))
            if (length(matched) == 0L) return(FALSE)
            matched[[1]]
        } else {
            filter
        }
        slot_val = tryCatch(S7::prop(node, slot_name), error = function(e) NULL)
        is.null(slot_val)
    }, param_terms)

    if (length(bad) > 0L) {
        slot_name = filter %||% {
            node = bad[[1]]$node
            cls = S7::S7_class(node)
            fmls = names(formals(cls@constructor))
            intersect(fmls, S7::prop_names(node))[[1]]
        }
        bad_labels = vapply(bad, function(t) {
            cls_name = S7::S7_class(t$node)@name
            x_lbl = rlang::as_label(t$node@x)
            paste0(cls_name, "(", x_lbl, ")")
        }, character(1))

        cli::cli_abort(c(
            "All parameter terms must specify {.arg {slot_name}} when used in this context.",
            "i" = "Ambiguous term{?s} found: {.and {.code {bad_labels}}}.",
            "i" = "Supply a condition, e.g. {.code MU(extra, group == \"1\")}."
        ))
    }

    nms = vapply(param_terms, function(t) extract_param_name(t$node), character(1))
    raw_coefs = vapply(param_terms, `[[`, numeric(1), "coef")
    names(raw_coefs) = nms

    unique_nms = unique(nms)
    coefs = vapply(unique_nms, function(nm) sum(raw_coefs[nms == nm]), numeric(1))
    names(coefs) = unique_nms

    zero_terms = names(coefs[coefs == 0])
    if (length(zero_terms) > 0L) {
        cli::cli_warn(c(
            "Zero-coefficient term{?s} in contrast: {.val {zero_terms}}.",
            "i" = "Duplicate parameters with opposite signs cancelled out.",
            "i" = "Verify the hypothesis is written as intended."
        ))
    }

    scalar_val = -Reduce("+", lapply(scalar_terms, `[[`, "value"), 0)

    op = claim@op
    lhs_has_only_scalars = !any(vapply(lhs_terms, function(t) t$kind == "param", logical(1)))
    if (lhs_has_only_scalars && length(lhs_terms) > 0L) {
        op = unname(FLIP_OP[op])
    }

    list(coefs = coefs, scalar = scalar_val, op = op)
}

#' Package resolved claim arguments for injection
#'
#' Used inside a `claim_translator` to declare argument names and values
#' merged into the impl's call. Names must match the formals of the impl's
#' `fn`.
#'
#' @param ... Named arguments to inject.
#'
#' @return A named list with class `"claim_args"`.
#'
#' @keywords internal
#' @noRd
claim_args = function(...) {
    args = list(...)
    if (length(args) == 0L || is.null(names(args)) || any(!nzchar(names(args)))) {
        cli::cli_abort("All arguments to {.fn claim_args} must be named.")
    }
    structure(args, class = "claim_args")
}

contains_param = function(node) {
    if (S7::S7_inherits(node, param_obj)) return(TRUE)
    if (inherits(node, "arith_node")) {
        return(any(vapply(node$operands, contains_param, logical(1))))
    }
    FALSE
}

assert_linear = function(node, call_nm) {
    if (!inherits(node, "arith_node")) return(invisible(NULL))

    op = node$op
    ops = node$operands

    if (op == "*") {
        if (contains_param(ops[[1]]) && contains_param(ops[[2]])) {
            cli::cli_abort(c(
                "Non-linear hypothesis detected: parameter multiplied by parameter.",
                "i" = "{.fn {call_nm}} only handles linear combinations of parameters.",
                "x" = "Found: {.code {deparse(node$expr)}}."
            ))
        }
    }

    if (op == "/") {
        if (contains_param(ops[[2]])) {
            cli::cli_abort(c(
                "Non-linear hypothesis detected: parameter in denominator.",
                "i" = "{.fn {call_nm}} only handles linear combinations of parameters.",
                "x" = "Found: {.code {deparse(node$expr)}}."
            ))
        }
    }

    if (op == "^") {
        if (contains_param(ops[[1]])) {
            cli::cli_abort(c(
                "Non-linear hypothesis detected: parameter raised to a power.",
                "i" = "{.fn {call_nm}} only handles linear combinations of parameters.",
                "x" = "Found: {.code {deparse(node$expr)}}."
            ))
        }
    }

    lapply(ops, assert_linear, call_nm = call_nm)
    invisible(NULL)
}

collect_terms = function(node, sign = 1L, coef = 1) {
    if (is.numeric(node)) {
        return(list(list(kind = "scalar", value = sign * coef * node, node = node)))
    }

    if (S7::S7_inherits(node, param_obj)) {
        return(list(list(kind = "param", coef = sign * coef, node = node)))
    }

    if (inherits(node, "arith_node")) {
        op = node$op
        ops = node$operands

        if (op == "+") {
            return(c(
                collect_terms(ops[[1]], sign, coef),
                collect_terms(ops[[2]], sign, coef)
            ))
        }

        if (op == "-") {
            if (length(ops) == 1L) return(collect_terms(ops[[1]], -sign, coef))
            return(c(
                collect_terms(ops[[1]], sign, coef),
                collect_terms(ops[[2]], -sign, coef)
            ))
        }

        if (op == "*") {
            if (is.numeric(ops[[1]])) return(collect_terms(ops[[2]], sign, coef * ops[[1]]))
            return(collect_terms(ops[[1]], sign, coef * ops[[2]]))
        }

        if (op == "/") {
            return(collect_terms(ops[[1]], sign, coef / ops[[2]]))
        }
    }

    cli::cli_abort(
        "Cannot reduce term to a linear combination: {.code {deparse(node$expr %||% node)}}."
    )
}

extract_param_name = function(node) {
    if (!S7::S7_inherits(node, param_obj)) {
        cli::cli_abort("Expected a param_obj node.")
    }

    if (S7::S7_inherits(node, RHO)) {
        return(paste0(rlang::as_label(node@x), "~", rlang::as_label(node@y)))
    }

    given = node@given

    if (!is.null(given)) {
        given_expr = rlang::quo_get_expr(given)
        if (rlang::is_call(given_expr, "==") && length(given_expr) == 3L) {
            return(as.character(given_expr[[3]]))
        }
        return(deparse(given_expr))
    }

    rlang::as_label(node@x)
}
