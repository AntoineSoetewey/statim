test_that("prepare() dispatches to prepare_test() for test functions", {
    out = sleep |>
        define_model(x_by(extra, group)) |>
        prepare(TTEST)

    expect_true(S7::S7_inherits(out, test_lazy))
})

test_that("prepare() dispatches to prepare_model() for model functions", {
    out = mtcars |>
        define_model(mpg ~ .) |>
        prepare(LINEAR_REG)

    expect_true(S7::S7_inherits(out, model_lazy))
})

test_that("prepare() and prepare_test() produce identical output", {
    dm = sleep |> define_model(x_by(extra, group))

    via_prepare = dm |> prepare(TTEST)
    via_alias = dm |> prepare_test(TTEST)

    expect_identical(via_prepare@test_spec, via_alias@test_spec)
    expect_identical(via_prepare@processed, via_alias@processed)
    expect_identical(via_prepare@var_id, via_alias@var_id)
})

test_that("prepare() and prepare_model() produce identical output", {
    dm = mtcars |> define_model(mpg ~ .)

    via_prepare = dm |> prepare(LINEAR_REG)
    via_alias = dm |> prepare_model(LINEAR_REG)

    expect_identical(via_prepare@model_spec, via_alias@model_spec)
    expect_identical(via_prepare@processed, via_alias@processed)
    expect_identical(via_prepare@var_id, via_alias@var_id)
})

test_that("prepare() errors on non-spec function", {
    dm = sleep |> define_model(x_by(extra, group))

    expect_error(
        dm |> prepare(mean),
        regexp = "must be a function built with",
        class = "rlang_error"
    )
})

test_that("prepare() errors when .fn returns a non-spec object", {
    dm = sleep |> define_model(x_by(extra, group))
    bad_fn = function(.var_id = NULL) list(x = 1)

    expect_error(
        dm |> prepare(bad_fn),
        regexp = "must return a",
        class = "rlang_error"
    )
})

test_that("prepare() works with via() after dispatch", {
    out = sleep |>
        define_model(x_by(extra, group)) |>
        prepare(TTEST) |>
        via("permute", n = 99L) |>
        conclude()

    expect_true(S7::S7_inherits(out, cld_exec))
})
