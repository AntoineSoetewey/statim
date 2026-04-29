test_that("via.test_lazy sets recalibrate_spec on test_lazy", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot", n = 200)

    expect_s3_class(lazy, "test_lazy")
    expect_false(is.null(lazy$recalibrate_spec))
    expect_equal(lazy$recalibrate_spec$method_name, "boot")
    expect_equal(lazy$recalibrate_spec$args$n, 200)
})

test_that("via.test_lazy passes user args correctly", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot", n = 500, seed = 1L)

    expect_equal(lazy$recalibrate_spec$args$n, 500)
    expect_equal(lazy$recalibrate_spec$args$seed, 1L)
})

test_that("via.test_lazy errors on unknown method", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST)

    expect_error(via(lazy, "nonexistent_method"))
})

test_that("via.test_lazy errors with available variants listed", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST)

    expect_error(
        via(lazy, "bayes"),
        regexp = "boot|permute"
    )
})

test_that("conclude respects via args", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot", n = 50, seed = 42) |>
        conclude()

    expect_equal(result$data$n, 50)
})
