# ---- gauge() on class_lm_object ----

test_that("gauge() on a linear model returns r_squared matching base R summary()", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    gauge_out = gauge(fit)
    base_r2 = summary(lm(dist ~ speed, data = cars))$r.squared

    expect_s3_class(gauge_out, "tbl_df")
    r2_row = gauge_out$value[gauge_out$metric == "r_squared"]
    expect_equal(r2_row, base_r2, tolerance = 1e-8)
})

test_that("gauge() cohens_f2 matches r_squared / (1 - r_squared)", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    gauge_out = gauge(fit)
    r2 = gauge_out$value[gauge_out$metric == "r_squared"]
    f2 = gauge_out$value[gauge_out$metric == "cohens_f2"]

    expect_equal(f2, r2 / (1 - r2), tolerance = 1e-8)
})

# ---- gauge() on class_glm_object ----

test_that("gauge() on a binomial GLM returns deviance_r2 matching 1 - deviance/null_deviance", {
    fit = mtcars |>
        define_model(am ~ wt + hp) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    gauge_out = gauge(fit)

    base_fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    manual_r2 = 1 - base_fit$deviance / base_fit$null.deviance

    r2_row = gauge_out$value[gauge_out$metric == "deviance_r2"]
    expect_equal(r2_row, manual_r2, tolerance = 1e-8)
})

test_that("gauge() on a binomial GLM returns mcfadden_r2 matching 1 - logLik/null_logLik", {
    fit = mtcars |>
        define_model(am ~ wt + hp) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    gauge_out = gauge(fit)

    base_fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    null_fit = glm(am ~ 1, data = mtcars, family = binomial())
    manual_r2 = 1 - as.numeric(logLik(base_fit)) / as.numeric(logLik(null_fit))

    r2_row = gauge_out$value[gauge_out$metric == "mcfadden_r2"]
    expect_equal(r2_row, manual_r2, tolerance = 1e-8)
})

test_that("gauge() cohens_f2 columns match r2 / (1 - r2) for both GLM r2 flavors", {
    fit = mtcars |>
        define_model(am ~ wt + hp) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    gauge_out = gauge(fit)
    deviance_r2 = gauge_out$value[gauge_out$metric == "deviance_r2"]
    mcfadden_r2 = gauge_out$value[gauge_out$metric == "mcfadden_r2"]
    f2_deviance = gauge_out$value[gauge_out$metric == "cohens_f2_deviance"]
    f2_mcfadden = gauge_out$value[gauge_out$metric == "cohens_f2_mcfadden"]

    expect_equal(f2_deviance, deviance_r2 / (1 - deviance_r2), tolerance = 1e-8)
    expect_equal(f2_mcfadden, mcfadden_r2 / (1 - mcfadden_r2), tolerance = 1e-8)
})

test_that("gauge() on a gaussian GLM deviance_r2 matches lm()'s r.squared", {
    fit = mtcars |>
        define_model(mpg ~ wt) |>
        prepare_model(GLM) |>
        conclude()

    gauge_out = gauge(fit)
    base_r2 = summary(lm(mpg ~ wt, data = mtcars))$r.squared

    r2_row = gauge_out$value[gauge_out$metric == "deviance_r2"]
    expect_equal(r2_row, base_r2, tolerance = 1e-6)
})

test_that("gauge() on a GLM returns exactly the four documented metrics", {
    fit = mtcars |>
        define_model(am ~ wt) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    gauge_out = gauge(fit)

    expect_setequal(
        gauge_out$metric,
        c("deviance_r2", "mcfadden_r2", "cohens_f2_deviance", "cohens_f2_mcfadden")
    )
    expect_equal(nrow(gauge_out), 4L)
})

# ---- gauge() on class_p_test ----

test_that("gauge() on a proportion test returns cohens_h against the stated null", {
    fit = define_model(prop(45, 100)) |>
        prepare_test(P_TEST) |>
        state_null(PI() == 0.5) |>
        conclude()

    gauge_out = gauge(fit)
    manual_h = 2 * asin(sqrt(45 / 100)) - 2 * asin(sqrt(0.5))

    expect_equal(gauge_out$metric, "cohens_h")
    expect_equal(gauge_out$value, manual_h, tolerance = 1e-8)
})

test_that("gauge() cohens_h falls back to 0.5 when no null was stated", {
    fit = define_model(prop(45, 100)) |>
        prepare_test(P_TEST) |>
        conclude()

    gauge_out = gauge(fit)
    manual_h = 2 * asin(sqrt(45 / 100)) - 2 * asin(sqrt(0.5))

    expect_equal(gauge_out$value, manual_h, tolerance = 1e-8)
})

# ---- gauge() on class_ttest_one ----

test_that("gauge() on a one-sample t-test returns cohens_d = t_stat / sqrt(n)", {
    fit = sleep |>
        define_model(on(extra)) |>
        prepare_test(T_TEST) |>
        conclude()

    gauge_out = gauge(fit)
    n = fit@data@df + 1
    manual_d = fit@data@t_stat / sqrt(n)

    expect_equal(gauge_out$metric, "cohens_d")
    expect_equal(gauge_out$value, manual_d, tolerance = 1e-8)
})

test_that("gauge() cohens_d on a one-sample t-test matches mean-difference-over-sd directly", {
    fit = sleep |>
        define_model(on(extra)) |>
        prepare_test(T_TEST) |>
        conclude()

    gauge_out = gauge(fit)
    manual_d = mean(sleep$extra) / sd(sleep$extra)

    expect_equal(gauge_out$value, manual_d, tolerance = 1e-8)
})

test_that("gauge() cohens_d handles the multi variant, one row per term", {
    fit = iris |>
        define_model(on(where(is.numeric))) |>
        prepare_test(T_TEST) |>
        via("multi") |>
        conclude()

    gauge_out = gauge(fit)

    expect_equal(nrow(gauge_out), length(fit@data@term))
    expect_equal(gauge_out$term, fit@data@term)
})

# ---- gauge() on unimplemented stat_infer subclasses ----

test_that("gauge() errors clearly when no auto_gauge method exists for a class_stat_infer subclass", {
    fit = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        conclude()

    # class_ttest_two has no auto_gauge method yet (n1/n2 not stored, see
    # conversation notes — implement once those slots exist, then update
    # this test to assert the actual cohens_d value instead of the error).
    expect_error(gauge(fit), "No .*auto_gauge.* method")
})
