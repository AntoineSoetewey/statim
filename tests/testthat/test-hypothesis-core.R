# ---- `null_claim` printing ----

test_that("print.null_claim: displays a binary comparison claim", {
    claim = parse_null_claim(rlang::quo(MU(extra) == 0))
    output = capture.output(print(claim))
    expect_true(any(grepl("Null Hypothesis", output)))
    expect_true(any(grepl("LHS", output)))
    expect_true(any(grepl("Op", output)))
    expect_true(any(grepl("RHS", output)))
    expect_true(any(grepl("MU\\(extra\\)", output)))
})

test_that("print.null_claim: displays an arithmetic LHS label", {
    claim = parse_null_claim(rlang::quo(2 * MU(extra) - MU(extra) == 0))
    output = capture.output(print(claim))
    expect_true(any(grepl("MU\\(extra\\)", output)))
})

test_that("print.null_claim: displays MU with a given condition", {
    claim = parse_null_claim(rlang::quo(MU(extra, group == "1") == 0))
    output = capture.output(print(claim))
    expect_true(any(grepl("MU\\(extra, group", output)))
})

test_that("print.null_claim: displays PI with x and given", {
    claim = parse_null_claim(rlang::quo(PI(success, group == "1") == 0.3))
    output = capture.output(print(claim))
    expect_true(any(grepl("PI\\(success", output)))
})

test_that("print.null_claim: displays PI with no arguments", {
    claim = parse_null_claim(rlang::quo(PI() == 0.5))
    output = capture.output(print(claim))
    expect_true(any(grepl("PI\\(\\)", output)))
})

test_that("print.null_claim: displays RHO with both variables", {
    claim = parse_null_claim(rlang::quo(RHO(speed, dist) == 0))
    output = capture.output(print(claim))
    expect_true(any(grepl("RHO\\(speed, dist\\)", output)))
})

# ---- `stated_null` printing ----

test_that("print.stated_null: displays test name, default method, and claim", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        state_null(MU(extra) == 0)

    output = capture.output(print(lazy))
    expect_true(any(grepl("Test Specification", output)))
    expect_true(any(grepl("T-Test", output)))
    expect_true(any(grepl("default", output)))
    expect_true(any(grepl("Null Hypothesis", output)))
})

test_that("print.stated_null: displays recalibrated method and args", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        state_null(MU(extra) == 0) |>
        via("boot", n = 2000)

    output = capture.output(print(lazy))
    expect_true(any(grepl("boot", output)))
    expect_true(any(grepl("n = 2000", output)))
})

# ---- `attach_claim_to_lazy` direct guard ----

test_that("attach_claim_to_lazy: rejects a non-test_lazy object directly", {
    claim = parse_null_claim(rlang::quo(MU(extra) == 0))
    expect_error(
        attach_claim_to_lazy(list(), claim),
        regexp = "test_lazy",
        class = "rlang_error"
    )
})

# ---- `claim_args` ----

test_that("claim_args: errors with no arguments", {
    expect_error(claim_args(), class = "rlang_error")
})

test_that("claim_args: errors when arguments are unnamed", {
    expect_error(claim_args(1, 2), class = "rlang_error")
})

test_that("claim_args: errors when some arguments are unnamed", {
    expect_error(claim_args(a = 1, 2), class = "rlang_error")
})

test_that("claim_args: returns a named list with claim_args class", {
    result = claim_args(.mu = 0, .alt = "two.sided")
    expect_true(inherits(result, "claim_args"))
    expect_equal(result$.mu, 0)
    expect_equal(result$.alt, "two.sided")
})

# ---- `map_claim` ----

test_that("map_claim: errors when all arguments are unnamed", {
    expect_error(
        map_claim(function(claim, processed) 1),
        class = "rlang_error"
    )
})

test_that("map_claim: errors when some arguments are unnamed", {
    expect_error(
        map_claim(.mu = function(claim, processed) 0, function(claim, processed) 1),
        class = "rlang_error"
    )
})

test_that("map_claim: errors when an argument is not a function", {
    expect_error(
        map_claim(.mu = 5),
        class = "rlang_error"
    )
})

test_that("map_claim: returns a function carrying the map_claim class", {
    parser = map_claim(.mu = function(claim, processed) 0)
    expect_true(inherits(parser, "map_claim"))
    expect_true(is.function(parser))
})

test_that("map_claim: resolved function dispatches resolvers and returns claim_args", {
    claim = parse_null_claim(rlang::quo(MU(extra) == 0))
    parser = map_claim(
        .mu = function(claim, processed) claim_scalar(claim)$scalar,
        .alt = function(claim, processed) "two.sided"
    )
    result = parser(claim, processed = list())
    expect_true(inherits(result, "claim_args"))
    expect_equal(result$.mu, 0)
    expect_equal(result$.alt, "two.sided")
})

# ---- `claim_scalar` ----

test_that("claim_scalar: resolves a simple equality claim", {
    claim = parse_null_claim(rlang::quo(MU(extra) == 5))
    result = claim_scalar(claim)
    expect_equal(result$scalar, 5)
    expect_equal(result$op, "==")
    expect_equal(unname(result$coefs), 1)
    expect_equal(names(result$coefs), "extra")
})

test_that("claim_scalar: retains the coefficient when solve_coef = FALSE", {
    claim = parse_null_claim(rlang::quo(2 * MU(extra) == 10))
    result = claim_scalar(claim)
    expect_equal(unname(result$coefs), 2)
    expect_equal(result$scalar, 10)
})

test_that("claim_scalar: solves for the parameter value when solve_coef = TRUE", {
    claim = parse_null_claim(rlang::quo(2 * MU(extra) == 10))
    result = claim_scalar(claim, solve_coef = TRUE)
    expect_equal(result$scalar, 5)
    expect_equal(unname(result$coefs), 1)
})

test_that("claim_scalar: errors when solving with a zero coefficient", {
    claim = parse_null_claim(rlang::quo(0 * MU(extra) == 5))
    expect_error(
        claim_scalar(claim, solve_coef = TRUE),
        regexp = "coefficient is zero"
    )
})

test_that("claim_scalar: errors when no parameter term is found", {
    claim = null_claim(lhs = 5, rhs = 3, op = "==", alt_op = "!=", expr = quote(5 == 3))
    expect_error(
        claim_scalar(claim),
        regexp = "No population parameter"
    )
})

test_that("claim_scalar: errors when more than one parameter term is found", {
    claim = parse_null_claim(rlang::quo(MU(extra) + MU(other) == 0))
    expect_error(
        claim_scalar(claim),
        regexp = "single parameter"
    )
})

test_that("claim_scalar: flips the operator when LHS contains only scalars", {
    p = MU(extra)
    claim = null_claim(lhs = 5, rhs = p, op = "<", alt_op = ">=", expr = quote(5 < MU(extra)))
    result = claim_scalar(claim)
    expect_equal(result$op, ">=")
    expect_equal(result$scalar, -5)
    expect_equal(unname(result$coefs), -1)
})

test_that("claim_scalar: aborts on an unreducible term", {
    # `node$expr %||% node` inside the abort message requires `node` to
    # support `$` safely. A bare atomic value (e.g. a string) would itself
    # error on `$` before `%||%` can fall back — so an unclassed list is
    # used here instead, to reach the intended "Cannot reduce" message.
    claim = null_claim(
        lhs = list(unrecognized = TRUE),
        rhs = MU(extra),
        op = "==",
        alt_op = "!=",
        expr = quote(x)
    )
    expect_error(
        claim_scalar(claim),
        regexp = "Cannot reduce"
    )
})

# ---- `claim_contrast_coefs` ----

test_that("claim_contrast_coefs: resolves two-group contrast coefficients", {
    claim = parse_null_claim(
        rlang::quo(2 * MU(extra, group == "1") - MU(extra, group == "2") == 4)
    )
    result = claim_contrast_coefs(claim, filter = "given")
    expect_equal(unname(result$coefs[["1"]]), 2)
    expect_equal(unname(result$coefs[["2"]]), -1)
    expect_equal(result$scalar, 4)
    expect_equal(result$op, "==")
})

test_that("claim_contrast_coefs: extracts the coefficient when numeric is the right operand", {
    claim = parse_null_claim(rlang::quo(MU(extra) * 2 == 10))
    result = claim_contrast_coefs(claim)
    expect_equal(unname(result$coefs), 2)
    expect_equal(result$scalar, 10)
})

test_that("claim_contrast_coefs: extracts the coefficient through division", {
    claim = parse_null_claim(rlang::quo(MU(extra) / 2 == 5))
    result = claim_contrast_coefs(claim)
    expect_equal(unname(result$coefs), 0.5)
    expect_equal(result$scalar, 5)
})

test_that("claim_contrast_coefs: errors when no parameter term is found", {
    claim = null_claim(lhs = 5, rhs = 3, op = "==", alt_op = "!=", expr = quote(5 == 3))
    expect_error(
        claim_contrast_coefs(claim),
        regexp = "No population parameter"
    )
})

test_that("claim_contrast_coefs: rejects a parameter multiplied by a parameter", {
    claim = parse_null_claim(rlang::quo(MU(extra) * MU(other) == 0))
    expect_error(
        claim_contrast_coefs(claim),
        regexp = "Non-linear hypothesis"
    )
})

test_that("claim_contrast_coefs: rejects a parameter in the denominator", {
    claim = parse_null_claim(rlang::quo(5 / MU(extra) == 0))
    expect_error(
        claim_contrast_coefs(claim),
        regexp = "denominator"
    )
})

test_that("claim_contrast_coefs: rejects a parameter raised to a power", {
    claim = parse_null_claim(rlang::quo(MU(extra)^2 == 0))
    expect_error(
        claim_contrast_coefs(claim),
        regexp = "raised to a power"
    )
})

test_that("claim_contrast_coefs: errors when a required slot is missing (explicit filter)", {
    claim = parse_null_claim(rlang::quo(MU(extra) == 0))
    expect_error(
        claim_contrast_coefs(claim, filter = "given"),
        regexp = "must specify"
    )
})

test_that("claim_contrast_coefs: auto-detects the required slot when filter is NULL", {
    claim = parse_null_claim(rlang::quo(PI() == 0.5))
    expect_error(
        claim_contrast_coefs(claim),
        regexp = "must specify"
    )
})

test_that("claim_contrast_coefs: warns when duplicate terms cancel to zero", {
    claim = parse_null_claim(
        rlang::quo(MU(extra, group == "1") - MU(extra, group == "1") == 0)
    )
    expect_warning(
        claim_contrast_coefs(claim),
        regexp = "cancelled out"
    )
})

test_that("claim_contrast_coefs: flips the operator when LHS contains only scalars", {
    p = MU(extra, group == "1")
    claim = null_claim(
        lhs = 3, rhs = p, op = ">", alt_op = "<=",
        expr = quote(3 > MU(extra, group == "1"))
    )
    result = claim_contrast_coefs(claim, filter = "given")
    expect_equal(result$op, "<=")
})

# ---- `extract_param_name` ----

test_that("extract_param_name: RHO returns an x~y label", {
    node = RHO(speed, dist)
    expect_equal(extract_param_name(node), "speed~dist")
})

test_that("extract_param_name: MU without given returns the x label", {
    node = MU(extra)
    expect_equal(extract_param_name(node), "extra")
})

test_that("extract_param_name: MU with an == given returns the RHS value", {
    node = MU(extra, group == "1")
    expect_equal(extract_param_name(node), "1")
})

test_that("extract_param_name: MU with a non-== given returns the deparsed expression", {
    node = MU(extra, group %in% c("1", "2"))
    expect_equal(extract_param_name(node), deparse(quote(group %in% c("1", "2"))))
})

test_that("extract_param_name: errors for a non-param_obj node", {
    expect_error(extract_param_name(5), class = "rlang_error")
})
