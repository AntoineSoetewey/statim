test_that("CORTEST() eager form returns stat_infer_spec", {
    result = CORTEST(rel(speed, dist), cars)

    expect_s7_class(result, stat_infer_spec)
})

test_that("CORTEST() eager result data is a class_corr_two", {
    result = CORTEST(rel(speed, dist), cars)

    expect_s7_class(result@data, class_corr_two)
})

test_that("CORTEST() eager result has expected slots populated", {
    result = CORTEST(rel(speed, dist), cars)

    expect_true(length(result@data@ind_vars) > 0L)
    expect_true(length(result@data@resp_vars) > 0L)
    expect_true(length(result@data@estimate) > 0L)
    expect_true(length(result@data@statistic) > 0L)
    expect_true(length(result@data@p_val) > 0L)
})

test_that("CORTEST() eager result matches base R cor.test()", {
    result = CORTEST(rel(speed, dist), cars)
    base = cor.test(cars$speed, cars$dist, method = "pearson")

    expect_equal(result@data@estimate[[1]], unname(base$estimate), tolerance = 1e-6)
    expect_equal(result@data@statistic[[1]], unname(base$statistic), tolerance = 1e-6)
    expect_equal(result@data@p_val[[1]], base$p.value, tolerance = 1e-6)
    expect_equal(result@data@lower_ci[[1]], base$conf.int[[1]], tolerance = 1e-6)
    expect_equal(result@data@upper_ci[[1]], base$conf.int[[2]], tolerance = 1e-6)
})

test_that("CORTEST() eager print returns invisibly", {
    result = CORTEST(rel(speed, dist), cars)

    expect_invisible(print(result))
})

test_that("classical pipeline returns cld_exec", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("classical pipeline result matches eager result numerically", {
    eager = CORTEST(rel(speed, dist), cars)
    pipeline = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        conclude()

    expect_equal(
        eager@data@estimate[[1]],
        pipeline@data@estimate[[1]],
        tolerance = 1e-6
    )
    expect_equal(
        eager@data@p_val[[1]],
        pipeline@data@p_val[[1]],
        tolerance = 1e-6
    )
})

test_that("base variant CI slots are populated for Pearson", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        conclude()

    expect_true(length(result@data@lower_ci) > 0L)
    expect_true(length(result@data@upper_ci) > 0L)
    expect_lt(result@data@lower_ci[[1]], result@data@upper_ci[[1]])
})

test_that("base variant df slot is populated for Pearson", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        conclude()

    expect_true(length(result@data@df) > 0L)
    expect_equal(result@data@df[[1]], nrow(cars) - 2L, tolerance = 1e-6)
})

test_that("spearman variant returns cld_exec with method = 'spearman'", {
    result = suppressWarnings(
        iris |>
            define_model(rel(Sepal.Length, Petal.Length)) |>
            prepare_test(CORTEST) |>
            via("spearman") |>
            conclude()
    )

    expect_s7_class(result, cld_exec)
    expect_equal(result@cld_meta$method, "spearman")
})

test_that("spearman variant result is a class_corr_two", {
    result = suppressWarnings(
        iris |>
            define_model(rel(Sepal.Length, Petal.Length)) |>
            prepare_test(CORTEST) |>
            via("spearman") |>
            conclude()
    )

    expect_s7_class(result@data, class_corr_two)
})

test_that("spearman variant matches base R cor.test()", {
    result = suppressWarnings(
        iris |>
            define_model(rel(Sepal.Length, Petal.Length)) |>
            prepare_test(CORTEST) |>
            via("spearman") |>
            conclude()
    )

    base = suppressWarnings(
        cor.test(iris$Sepal.Length, iris$Petal.Length, method = "spearman")
    )

    expect_equal(result@data@estimate[[1]], unname(base$estimate), tolerance = 1e-6)
    expect_equal(result@data@p_val[[1]], base$p.value, tolerance = 1e-6)
})

test_that("spearman variant has no CI or df slots", {
    result = suppressWarnings(
        iris |>
            define_model(rel(Sepal.Length, Petal.Length)) |>
            prepare_test(CORTEST) |>
            via("spearman") |>
            conclude()
    )

    expect_length(result@data@lower_ci, 0L)
    expect_length(result@data@upper_ci, 0L)
    expect_length(result@data@df, 0L)
})

test_that("kendall variant returns cld_exec with method = 'kendall'", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        via("kendall") |>
        conclude()

    expect_s7_class(result, cld_exec)
    expect_equal(result@cld_meta$method, "kendall")
})

test_that("kendall variant matches base R cor.test()", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        via("kendall") |>
        conclude()

    base = cor.test(cars$speed, cars$dist, method = "kendall")

    expect_equal(result@data@estimate[[1]], unname(base$estimate), tolerance = 1e-6)
    expect_equal(result@data@p_val[[1]], base$p.value, tolerance = 1e-6)
})

test_that("kendall variant has no CI or df slots", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        via("kendall") |>
        conclude()

    expect_length(result@data@lower_ci, 0L)
    expect_length(result@data@upper_ci, 0L)
    expect_length(result@data@df, 0L)
})

test_that("non-zero .rho triggers Fisher-z path", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        update(.rho = 0.5) |>
        conclude()

    base_result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        conclude()

    expect_false(
        isTRUE(all.equal(result@data@statistic, base_result@data@statistic))
    )
})

test_that("Fisher-z result p_val is in [0, 1]", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        update(.rho = 0.5) |>
        conclude()

    expect_gte(result@data@p_val[[1]], 0)
    expect_lte(result@data@p_val[[1]], 1)
})

test_that("Fisher-z result CI lower is less than upper", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        update(.rho = 0.5) |>
        conclude()

    expect_lt(result@data@lower_ci[[1]], result@data@upper_ci[[1]])
})

test_that("Fisher-z errors on |.rho| >= 1", {
    expect_error(
        cars |>
            define_model(rel(speed, dist)) |>
            prepare_test(CORTEST) |>
            update(.rho = 1) |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("Fisher-z errors on n < 4", {
    expect_error(
        CORTEST(rel(speed, dist), cars[1:3, ]),
        class = "rlang_error"
    )
})

test_that("state_null() with RHO == 0 runs through to conclude()", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        state_null(RHO(speed, dist) == 0) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("state_null() with non-zero scalar runs through to conclude()", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        state_null(RHO(speed, dist) >= 0.5) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("state_null() result matches manual .rho injection numerically", {
    via_state_null = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        state_null(RHO(speed, dist) >= 0.5) |>
        conclude()

    via_arg = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        update(.rho = 0.5, .alt = "less") |>
        conclude()

    expect_equal(
        via_state_null@data@statistic[[1]],
        via_arg@data@statistic[[1]],
        tolerance = 1e-6
    )
    expect_equal(
        via_state_null@data@p_val[[1]],
        via_arg@data@p_val[[1]],
        tolerance = 1e-6
    )
})

test_that("state_null() with scaled RHO solves through to conclude()", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        state_null(2 * RHO(speed, dist) >= 0.8) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("state_null() with invalid scalar errors", {
    expect_error(
        cars |>
            define_model(rel(speed, dist)) |>
            prepare_test(CORTEST) |>
            state_null(2 + RHO(speed, dist) >= 0.8) |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("state_null() with unsupported param type errors", {
    expect_error(
        cars |>
            define_model(rel(speed, dist)) |>
            prepare_test(CORTEST) |>
            state_null(MU(speed) == 0) |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("state_null() on spearman variant errors", {
    expect_error(
        cars |>
            define_model(rel(speed, dist)) |>
            prepare_test(CORTEST) |>
            via("spearman") |>
            state_null(RHO(speed, dist) == 0) |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("multi variant returns cld_exec with method = 'multi'", {
    result = mtcars |>
        define_model(rel(c(wt, hp), mpg)) |>
        prepare_test(CORTEST) |>
        via("multi") |>
        conclude()

    expect_s7_class(result, cld_exec)
    expect_equal(result@cld_meta$method, "multi")
})

test_that("multi variant result is a class_corr_two with multiple rows", {
    result = mtcars |>
        define_model(rel(c(wt, hp), mpg)) |>
        prepare_test(CORTEST) |>
        via("multi") |>
        conclude()

    expect_s7_class(result@data, class_corr_two)
    expect_length(result@data@ind_vars, 2L)
    expect_equal(result@data@ind_vars, c("wt", "hp"))
    expect_true(all(result@data@resp_vars == "mpg"))
})

test_that("multi variant matches base R cor.test() per variable", {
    result = mtcars |>
        define_model(rel(c(wt, hp), mpg)) |>
        prepare_test(CORTEST) |>
        via("multi") |>
        conclude()

    base_wt = cor.test(mtcars$wt, mtcars$mpg, method = "pearson")
    base_hp = cor.test(mtcars$hp, mtcars$mpg, method = "pearson")

    expect_equal(result@data@estimate[[1]], unname(base_wt$estimate), tolerance = 1e-6)
    expect_equal(result@data@estimate[[2]], unname(base_hp$estimate), tolerance = 1e-6)
    expect_equal(result@data@p_val[[1]], base_wt$p.value, tolerance = 1e-6)
    expect_equal(result@data@p_val[[2]], base_hp$p.value, tolerance = 1e-6)
})

test_that("multi variant respects .cor_type for all variables", {
    result = suppressWarnings(
        mtcars |>
            define_model(rel(c(wt, hp), mpg)) |>
            prepare_test(CORTEST) |>
            via("multi") |>
            update(.cor_type = "spearman") |>
            conclude()
    )

    base_wt = suppressWarnings(cor.test(mtcars$wt, mtcars$mpg, method = "spearman"))
    base_hp = suppressWarnings(cor.test(mtcars$hp, mtcars$mpg, method = "spearman"))

    expect_equal(result@data@estimate[[1]], unname(base_wt$estimate), tolerance = 1e-6)
    expect_equal(result@data@estimate[[2]], unname(base_hp$estimate), tolerance = 1e-6)
    expect_length(result@data@lower_ci, 0L)
    expect_length(result@data@df, 0L)
})

test_that("multi variant has CI and df populated for pearson", {
    result = mtcars |>
        define_model(rel(c(wt, hp), mpg)) |>
        prepare_test(CORTEST) |>
        via("multi") |>
        conclude()

    expect_length(result@data@lower_ci, 2L)
    expect_length(result@data@upper_ci, 2L)
    expect_length(result@data@df, 2L)
})

test_that("multi variant rejects multi-variable resp", {
    expect_error(
        mtcars |>
            define_model(rel(wt, c(mpg, hp))) |>
            prepare_test(CORTEST) |>
            via("multi") |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("state_null() on multi variant errors", {
    expect_error(
        mtcars |>
            define_model(rel(c(wt, hp), mpg)) |>
            prepare_test(CORTEST) |>
            via("multi") |>
            state_null(RHO(wt, mpg) == 0) |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("multi-x errors on rel()", {
    expect_error(
        CORTEST(rel(c(speed, dist), dist), cars),
        class = "rlang_error"
    )
})

test_that("tidy() on classical pipeline result returns a tibble", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
})

test_that("tidy() on classical pipeline result has expected columns", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        conclude() |>
        tidy()

    expect_true(all(c("pair", "estimate", "statistic", "p_val") %in% names(result)))
})

test_that("tidy() includes CI columns for Pearson", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        conclude() |>
        tidy()

    expect_true(any(grepl("^lower_", names(result))))
    expect_true(any(grepl("^upper_", names(result))))
})

test_that("tidy() omits CI columns for spearman", {
    result = suppressWarnings(
        iris |>
            define_model(rel(Sepal.Length, Petal.Length)) |>
            prepare_test(CORTEST) |>
            via("spearman") |>
            conclude() |>
            tidy()
    )

    expect_false(any(grepl("^lower_", names(result))))
    expect_false(any(grepl("^upper_", names(result))))
})
