new_gauge_test_stat = function(cls_name) {
    HTEST_FN(cls = cls_name, defs = list(), .name = "Gauge Unit Test Stat")
}

fake_exec = function(data, impl_cls, method = "default") {
    cld_exec(
        data = data,
        impl_cls = impl_cls,
        stat_cls = "irrelevant",
        print_fn = NULL,
        name = "irrelevant",
        cld_meta = list(method = method)
    )
}

clear_gauge_key = function(key) {
    if (exists(key, envir = register_gauge, inherits = FALSE)) {
        rm(list = key, envir = register_gauge)
    }
}

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
        c(
            "deviance_r2",
            "mcfadden_r2",
            "cohens_f2_deviance",
            "cohens_f2_mcfadden"
        )
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

# ---- method_gauge() constructor ----

test_that("method_gauge() accepts a default function with no variants", {
    mg = method_gauge(default = function(.x, ...) NULL)

    expect_true(is.function(mg@default))
    expect_equal(mg@variants, list())
})

test_that("method_gauge() accepts a default plus named variants", {
    mg = method_gauge(
        default = function(.x, ...) NULL,
        boot = function(.x, ...) NULL
    )

    expect_named(mg@variants, "boot")
})

test_that("method_gauge() rejects a non-function, non-NULL default", {
    expect_error(
        method_gauge(default = "not_a_function"),
        "must be a function or"
    )
})

test_that("method_gauge() rejects non-function variants", {
    expect_error(
        method_gauge(default = function(.x, ...) NULL, boot = "not_a_function"),
        "must be functions"
    )
})

# ---- making_gauge() ----

test_that("making_gauge() carries the stat function and model_type untouched", {
    stat_fn = new_gauge_test_stat("gauge_ut_build")
    call_obj = making_gauge(stat_fn, x_by)

    expect_s3_class(call_obj, "making_gauge_call")
    expect_identical(call_obj$obj, stat_fn)
    expect_identical(call_obj$model_type, x_by)
})

# ---- %<-% / making_gauge_register(): input validation ----

test_that("%<-% rejects a stat function not built with HTEST_FN/MODEL_FN", {
    plain_fn = function() NULL

    expect_error(
        making_gauge(plain_fn, x_by) %<-%
            method_gauge(default = function(.x, ...) NULL),
        "must be a function built with"
    )
})

test_that("%<-% rejects a model_type that isn't a var_id subclass or a formula", {
    stat_fn = new_gauge_test_stat("gauge_ut_bad_model_type")

    expect_error(
        making_gauge(stat_fn, numeric) %<-%
            method_gauge(default = function(.x, ...) NULL),
        "must be a class inheriting from"
    )
})

test_that("%<-% accepts S7::class_formula as a model_type", {
    stat_fn = new_gauge_test_stat("gauge_ut_formula_model_type")
    on.exit(clear_gauge_key("gauge_ut_formula_model_type_formula"))

    expect_no_error(
        making_gauge(stat_fn, S7::class_formula) %<-%
            method_gauge(default = function(.x, ...) NULL)
    )
})

test_that("%<-% rejects a right-hand side that isn't a method_gauge object", {
    stat_fn = new_gauge_test_stat("gauge_ut_bad_rhs")

    expect_error(
        making_gauge(stat_fn, x_by) %<-%
            list(default = function(.x, ...) NULL),
        "Right-hand side of"
    )
})

# ---- making_gauge_register(): fresh registration vs. merge ----

test_that("registering a new key stores exactly what was declared", {
    stat_fn = new_gauge_test_stat("gauge_ut_fresh")
    on.exit(clear_gauge_key("gauge_ut_fresh_x_by"))

    making_gauge(stat_fn, x_by) %<-%
        method_gauge(
            default = function(.x, ...) tibble::tibble(metric = "d", value = 1)
        )

    fake = fake_exec(list(), "gauge_ut_fresh_x_by", "default")
    expect_equal(gauge(fake)$value, 1)
})

test_that("a later registration on the same key keeps the old default when the new one omits it", {
    stat_fn = new_gauge_test_stat("gauge_ut_merge")
    on.exit(clear_gauge_key("gauge_ut_merge_x_by"))

    making_gauge(stat_fn, x_by) %<-%
        method_gauge(
            default = function(.x, ...) tibble::tibble(metric = "d", value = 1)
        )
    making_gauge(stat_fn, x_by) %<-%
        method_gauge(
            boot = function(.x, ...) tibble::tibble(metric = "d", value = 2)
        )

    default_out = gauge(fake_exec(list(), "gauge_ut_merge_x_by", "default"))
    boot_out = gauge(fake_exec(list(), "gauge_ut_merge_x_by", "boot"))

    expect_equal(default_out$value, 1)
    expect_equal(boot_out$value, 2)
})

test_that("a later registration on the same key overwrites the default when a new one is supplied", {
    stat_fn = new_gauge_test_stat("gauge_ut_overwrite")
    on.exit(clear_gauge_key("gauge_ut_overwrite_x_by"))

    making_gauge(stat_fn, x_by) %<-%
        method_gauge(
            boot = function(.x, ...) tibble::tibble(metric = "d", value = 2)
        )
    making_gauge(stat_fn, x_by) %<-%
        method_gauge(
            default = function(.x, ...) tibble::tibble(metric = "d", value = 3)
        )

    default_out = gauge(fake_exec(list(), "gauge_ut_overwrite_x_by", "default"))
    boot_out = gauge(fake_exec(list(), "gauge_ut_overwrite_x_by", "boot"))

    expect_equal(default_out$value, 3)
    expect_equal(boot_out$value, 2)
})

# ---- gauge() dispatch on non-class_stat_infer data: error branches ----

test_that("gauge() aborts when no gauge method is registered for impl_cls at all", {
    fake = fake_exec(list(), "gauge_ut_never_registered", "default")

    expect_error(gauge(fake), "No gauge method found")
})

test_that("gauge() resolves a named variant when one is registered", {
    stat_fn = new_gauge_test_stat("gauge_ut_variant_hit")
    on.exit(clear_gauge_key("gauge_ut_variant_hit_x_by"))

    making_gauge(stat_fn, x_by) %<-%
        method_gauge(
            default = function(.x, ...) tibble::tibble(metric = "d", value = 1),
            permute = function(.x, ...) tibble::tibble(metric = "d", value = 9)
        )

    fake = fake_exec(list(), "gauge_ut_variant_hit_x_by", "permute")
    expect_equal(gauge(fake)$value, 9)
})

test_that("gauge() aborts when the variant name has no matching entry", {
    stat_fn = new_gauge_test_stat("gauge_ut_variant_miss")
    on.exit(clear_gauge_key("gauge_ut_variant_miss_x_by"))

    making_gauge(stat_fn, x_by) %<-%
        method_gauge(
            default = function(.x, ...) tibble::tibble(metric = "d", value = 1)
        )

    fake = fake_exec(list(), "gauge_ut_variant_miss_x_by", "not_a_real_variant")
    expect_error(gauge(fake), "No gauge entry for variant")
})

test_that("gauge() aborts when method is \"default\" but no default function was registered", {
    stat_fn = new_gauge_test_stat("gauge_ut_no_default")
    on.exit(clear_gauge_key("gauge_ut_no_default_x_by"))

    making_gauge(stat_fn, x_by) %<-%
        method_gauge(
            boot = function(.x, ...) tibble::tibble(metric = "d", value = 2)
        )

    fake = fake_exec(list(), "gauge_ut_no_default_x_by", "default")
    expect_error(gauge(fake), "gauge function registered")
})
