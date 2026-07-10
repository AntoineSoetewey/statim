test_that("T_TEST() eager form returns stat_infer_spec", {
    result = T_TEST(x_by(extra, group), sleep)

    expect_s7_class(result, stat_infer_spec)
})

test_that("T_TEST() eager result data for `on()` is a class_ttest_one", {
    result = T_TEST(on(Sepal.Length), .mu = 5.8, iris)

    expect_s7_class(result@data, class_ttest_one)
})

test_that("T_TEST() eager result data for `x_by()` is a class_ttest_two", {
    result = T_TEST(x_by(extra, group), sleep)

    expect_s7_class(result@data, class_ttest_two)
})

test_that("T_TEST() eager result data for `pairwise()` is a class_ttest_pairwise", {
    result = T_TEST(pairwise(where(is.numeric)), iris)

    expect_s7_class(result@data, class_ttest_pairwise)
})

test_that("T_TEST() eager result data has expected slots", {
    result = T_TEST(x_by(extra, group), sleep)

    expect_true(length(result@data@group) > 0L)
    expect_true(length(result@data@t_stat) > 0L)
    expect_true(length(result@data@p_val) > 0L)
})

test_that("T_TEST() eager result matches base R t.test()", {
    result = T_TEST(x_by(extra, group), sleep)
    base = t.test(extra ~ group, data = sleep)

    expect_equal(
        result@data@t_stat[[1]],
        unname(base$statistic),
        tolerance = 1e-6
    )
    expect_equal(result@data@p_val[[1]], base$p.value, tolerance = 1e-6)
})

test_that("T_TEST() eager print returns invisibly", {
    out1 = T_TEST(on(extra), sleep)
    out2 = T_TEST(x_by(extra, group), sleep)
    out3 = T_TEST(pairwise(where(is.numeric)), iris)

    expect_invisible(print(out1))
    expect_invisible(print(out2))
    expect_invisible(print(out3))
})

test_that("classical pipeline returns cld_exec", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("classical pipeline result matches eager result numerically", {
    eager = T_TEST(x_by(extra, group), sleep)
    pipeline = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        conclude()

    expect_equal(
        eager@data@t_stat[[1]],
        pipeline@data@t_stat[[1]],
        tolerance = 1e-6
    )
})

test_that("classical pipeline with .paired = TRUE runs without error", {
    expect_no_error(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(T_TEST) |>
            update(.paired = TRUE) |>
            conclude()
    )
})

test_that("classical pipeline with wrong number of groups errors", {
    expect_error(
        iris |>
            define_model(x_by(Sepal.Length, Species)) |>
            prepare_test(T_TEST) |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("permute variant returns cld_exec with method = 'permute'", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        via("permute", n = 200L, seed = 1L) |>
        conclude()

    expect_s7_class(result, cld_exec)
    expect_equal(result@cld_meta$method, "permute")
})

test_that("permute variant result contains observed, null_dist, p.value, n", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        via("permute", n = 200L, seed = 1L) |>
        conclude()

    expect_named(
        result@data,
        c("observed", "null_dist", "p.value", "n"),
        ignore.order = TRUE
    )
    expect_length(result@data$null_dist, 200L)
})

test_that("permute variant is reproducible with seed", {
    run = function() {
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(T_TEST) |>
            via("permute", n = 200L, seed = 42L) |>
            conclude()
    }

    expect_equal(run()@data$p.value, run()@data$p.value)
})

test_that("permute variant p.value is in [0, 1]", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        via("permute", n = 500L, seed = 1L) |>
        conclude()

    expect_gte(result@data$p.value, 0)
    expect_lte(result@data$p.value, 1)
})

test_that("boot variant returns cld_exec with method = 'boot'", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        via("boot", n = 200L, seed = 1L) |>
        conclude()

    expect_s7_class(result, cld_exec)
    expect_equal(result@cld_meta$method, "boot")
})

test_that("boot variant result contains boot_dist, ci, n", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        via("boot", n = 200L, seed = 1L) |>
        conclude()

    expect_named(result@data, c("boot_dist", "ci", "n"), ignore.order = TRUE)
    expect_length(result@data$boot_dist, 200L)
    expect_length(result@data$ci, 2L)
})

test_that("boot variant ci lower is less than upper", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        via("boot", n = 500L, seed = 1L) |>
        conclude()

    expect_lt(result@data$ci[[1]], result@data$ci[[2]])
})

test_that("boot variant is reproducible with seed", {
    run = function() {
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(T_TEST) |>
            via("boot", n = 200L, seed = 7L) |>
            conclude()
    }

    expect_equal(run()@data$ci, run()@data$ci)
})

test_that("multi variant works on uneven selected data under `on()`", {
    set.seed(123)
    x1 = rnorm(29, 1, 1.5)
    x2 = rnorm(30, 1, 1.5)
    x3 = rnorm(31, 1, 1.5)

    expect_no_error({
        define_model(on(x1, x2, x3)) |>
            prepare(T_TEST) |>
            via("multi") |>
            conclude()
    })
})

test_that("T_TEST base errs cleanly when on() carries more than one variable", {
    expect_error(
        iris |>
            define_model(on(Sepal.Length, Sepal.Width)) |>
            prepare_test(T_TEST) |>
            conclude(),
        "requires exactly 1 variable"
    )
})

test_that("T_TEST base works with on() against a data.frame", {
    out = T_TEST(on(Sepal.Length), .mu = 5.8, iris)

    expect_s7_class(out@data, class_ttest_one)
    expect_equal(out@data@term, "Sepal.Length")
})

test_that("T_TEST base works with on() against a plain list", {
    data_list = list(x = c(5.1, 4.9, 4.7, 4.6, 5.0))
    out = T_TEST(on(x), .mu = 5.0, data_list)

    expect_s7_class(out@data, class_ttest_one)
    expect_equal(out@data@term, "x")
})

test_that("T_TEST multi variant works with unequal-length variables via on()", {
    data_list = list(
        x1 = c(83, 91, 94, 89, 89, 96, 91, 92, 90),
        x2 = c(91, 90, 81, 83, 84, 83, 88, 91, 89, 84),
        x3 = c(101, 100, 91, 93, 96, 95, 94)
    )
    out = iris |>
        define_model(on(where(is.numeric))) |>
        prepare_test(T_TEST) |>
        via("multi") |>
        conclude()

    expect_length(out@data@term, 4)

    out_list = define_model(on(x1, x2, x3), data_list) |>
        prepare_test(T_TEST) |>
        via("multi") |>
        conclude()

    expect_length(out_list@data@term, 3)
    expect_equal(out_list@data@term, c("x1", "x2", "x3"))
})

test_that("T_TEST multi variant .mu recycling still matches n_vars via length(), not ncol()", {
    data_list = list(x1 = c(1, 2, 3), x2 = c(4, 5, 6, 7))

    out = define_model(on(x1, x2), data_list) |>
        prepare_test(T_TEST) |>
        via("multi", .mu = c(1, 2)) |>
        conclude()
    expect_length(out@data@true_mu, 2)
})

test_that("contrast variant returns cld_exec", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        via("contrast") |>
        conclude()

    expect_s7_class(result, cld_exec)
    expect_equal(result@cld_meta$method, "contrast")
})

test_that("contrast variant result is a class_ttest_two", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        via("contrast") |>
        conclude()

    expect_s7_class(result@data, class_ttest_two)
    expect_true(length(result@data@t_stat) > 0L)
    expect_true(length(result@data@p_val) > 0L)
    expect_length(result@data@lower_ci, length(result@data@group))
})

test_that("state_null() with two-sample MU difference runs through to conclude()", {
    result = sleep |>
        define_model(extra %by% group) |>
        prepare_test(T_TEST) |>
        state_null(MU(extra, group == "1") - MU(extra, group == "2") >= 0) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("contrast variant with wrong number of groups errors", {
    expect_error(
        iris |>
            define_model(x_by(Sepal.Length, Species)) |>
            prepare_test(T_TEST) |>
            via("contrast") |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("state_null() with bare MU (no given) errors for x_by()", {
    expect_error(
        sleep |>
            define_model(extra %by% group) |>
            prepare_test(T_TEST) |>
            state_null(MU(extra) >= 0) |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("state_null() with unsupported param type errors", {
    expect_error(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(T_TEST) |>
            state_null(PI(extra) == 0.5) |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("tidy() on classical pipeline result returns a tibble", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
})

test_that("tidy() on classical pipeline result has expected columns", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        conclude() |>
        tidy()

    expect_true(all(
        c("group", "estimate", "t_stat", "p_val") %in% names(result)
    ))
})

test_that("tidy() on boot variant returns a tibble", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        via("boot") |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
})

test_that("tidy() on boot variant returns a tibble with lower and upper", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        via("boot", n = 200L, seed = 1L) |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
    expect_true(all(c("lower", "upper") %in% names(result)))
})

test_that("tidy() on contrast variant returns a tibble with expected columns", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        via("contrast") |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
    expect_true(all(c("t_stat", "p_val") %in% names(result)))
})

test_that("classical pipeline with via two_sample runs without error", {
    expect_no_error(
        ToothGrowth |>
            with(define_model(on(
                I(oj = len[supp == "OJ"]),
                I(vc = len[supp == "VC"])
            ))) |>
            prepare_test(T_TEST) |>
            via("two_sample") |>
            conclude()
    )
})

test_that("tidy() on two_sample variant returns a tibble with expected columns", {
    tidy_two_sample = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "OJ"]),
            I(vc = len[supp == "VC"])
        ))) |>
        prepare_test(T_TEST) |>
        via("two_sample") |>
        conclude() |>
        tidy()

    expect_s3_class(tidy_two_sample, "tbl_df")
    expect_true(all(
        c("group", "estimate", "t_stat", "p_val") %in% names(tidy_two_sample)
    ))
})

test_that("two_sample on() and default x_by() agree on the un-weighted two-sample case", {
    on_two_sample = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "VC"]),
            I(vc = len[supp == "OJ"])
        ))) |>
        prepare_test(T_TEST) |>
        via("two_sample") |>
        conclude()

    xby_default = ToothGrowth |>
        define_model(x_by(len, supp)) |>
        prepare_test(T_TEST) |>
        conclude()

    expect_equal(
        on_two_sample@data@t_stat,
        xby_default@data@t_stat,
        tolerance = 1e-8
    )
    expect_equal(on_two_sample@data@df, xby_default@data@df, tolerance = 1e-8)
    expect_equal(
        on_two_sample@data@p_val,
        xby_default@data@p_val,
        tolerance = 1e-8
    )
})

test_that("two_sample on() and contrast x_by() agree on an equivalent weighted hypothesis", {
    on_two_sample = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "OJ"]),
            I(vc = len[supp == "VC"])
        ))) |>
        prepare_test(T_TEST) |>
        via("two_sample") |>
        state_null(MU(oj) - 1 == 2 * MU(vc) - 3) |>
        conclude() |>
        tidy()

    xby_contrast = ToothGrowth |>
        define_model(x_by(len, supp)) |>
        prepare_test(T_TEST) |>
        via("contrast") |>
        state_null(
            MU(len, supp == "OJ") - 1 == 2 * MU(len, supp == "VC") - 3
        ) |>
        conclude() |>
        tidy()

    expect_equal(
        on_two_sample$estimate,
        xby_contrast$estimate,
        tolerance = 1e-8
    )
    expect_equal(on_two_sample$t_stat, xby_contrast$t_stat, tolerance = 1e-8)
    expect_equal(on_two_sample$df, xby_contrast$df, tolerance = 1e-8)
    expect_equal(on_two_sample$p_val, xby_contrast$p_val, tolerance = 1e-8)
    expect_equal(
        on_two_sample$lower_95,
        xby_contrast$lower_95,
        tolerance = 1e-8
    )
    expect_equal(
        on_two_sample$upper_95,
        xby_contrast$upper_95,
        tolerance = 1e-8
    )
})

# ---- Effect sizes ----

test_that("gauge() on x_by() two-sample t-test returns an approximate cohens_d with a message", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        conclude()

    expect_message(
        {
            gauge_out = gauge(result, quiet = FALSE)
        },
        "approximated"
    )
    expect_equal(gauge_out$metric, "cohens_d_approx")

    manual_d = 2 * result@data@t_stat / sqrt(result@data@df)
    expect_equal(gauge_out$value, manual_d, tolerance = 1e-8)
})

test_that("gauge() with quiet = TRUE suppresses the approximation message but keeps cohens_d_approx", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        conclude()

    expect_no_message({
        gauge_out = gauge(result, quiet = TRUE)
    })
    expect_equal(gauge_out$metric, "cohens_d_approx")
})

test_that("gauge() on pairwise() one-sample mode emits no message even without quiet", {
    result = T_TEST(
        pairwise(Sepal.Length, Sepal.Width, direction = "eq"),
        iris
    )@data

    expect_no_message({
        gauge_out = auto_gauge(result, quiet = TRUE)
    })
    expect_true(all(gauge_out$metric == "cohens_d"))
})

test_that("gauge() on pairwise() two-sample mode emits a message unless quiet = TRUE", {
    result = T_TEST(
        pairwise(Sepal.Length, Sepal.Width, Petal.Length),
        iris
    )@data

    expect_message(auto_gauge(result, quiet = FALSE), "approximated")
    expect_no_message(auto_gauge(result, quiet = TRUE))
})
