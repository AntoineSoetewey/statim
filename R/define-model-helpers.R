c_quo_vars_extract = function(x) {
    x_expr = rlang::quo_get_expr(x)

    if (is.symbol(x_expr)) {
        list(x_expr)
    } else if (rlang::is_call(x_expr, "c")) {
        as.list(x_expr[-1])
    } else {
        cli::cli_abort("Input must be a single variable or a {.code c()} call")
    }
}

c_vars_label = function(x) {
    as.character(x)
}

global_vars_data = function(vars) {
    mask = rlang::as_list(rlang::current_env())
    vctrs::new_data_frame(
        rlang::set_names(
            lapply(
                vars,
                \(v) rlang::eval_tidy(rlang::inject(v), env = mask)
            ),
            c_vars_label(vars)
        )
    )
}

two_vars_extract = function(args, data = NULL) {
    if (length(args) != 2L) {
        cli::cli_abort("This model only needs 2 arguments")
    }

    # ---- First Var ----
    # The argument is already captured in quosure
    x1 = args[[1]]
    x1_vars = c_quo_vars_extract(x1)

    # ---- Second Var ----
    # Same goes to the second argument
    x2 = args[[2]]
    x2_vars = c_quo_vars_extract(x2)

    if (rlang::is_null(data)) {
        x1_data = global_vars_data(x1_vars)
        x2_data = global_vars_data(x2_vars)
    } else {
        x1_data = dplyr::select({{ data }}, {{ x1 }})
        x2_data = dplyr::select({{ data }}, {{ x2 }})
    }

    list(
        x1_data = x1_data,
        x2_data = x2_data
    )
}

pairwise_data_extract = function(args, data = NULL) {
    selected_vars = args$args$dots
    direction = args$direction

    if (rlang::is_null(data) || is.environment(data)) {
        var_quos = rlang::as_quosures(
            selected_vars,
            env = rlang::current_env()
        )[-1]
        var_names = vapply(var_quos, rlang::as_label, FUN.VALUE = character(1))

        selected_data = tibble::new_tibble(
            rlang::set_names(
                lapply(var_quos, \(v) rlang::eval_tidy(v, env = rlang::current_env())),
                var_names
            )
        )
    } else {
        cols = tidyselect::eval_select(
            # rlang::expr(c(!!!selected_vars)),
            selected_vars,
            data = data
        )
        selected_data = data[, cols, drop = FALSE]
        var_names = names(cols)
    }

    pairs = pairs_generator(
        var_names,
        direction = direction,
        simplify = TRUE
    )

    list(
        var_names = var_names,
        pairs = pairs,
        data = selected_data
    )
}
