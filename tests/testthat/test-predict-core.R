new_fake_predict_model = function(cls_name, with_variant = FALSE) {
    slots = list(
        base = baseline(fn = function(.proc, ...) {
            list(coef = 1, note = "base")
        })
    )
    if (with_variant) {
        slots$alt = variant(fn = function(.proc, ...) {
            list(coef = 2, note = "alt")
        })
    }

    def = model_infer_define(
        model_type = S7::class_formula,
        impl = do.call(agendas, slots)
    )

    MODEL_FN(
        cls = cls_name,
        defs = list(def),
        .name = "Predict Unit Test Model"
    )
}

fit_default = function(model_fn) {
    mtcars |>
        define_model(mpg ~ wt) |>
        prepare_model(model_fn) |>
        conclude()
}

fit_variant = function(model_fn, method_name, ...) {
    mtcars |>
        define_model(mpg ~ wt) |>
        prepare_model(model_fn) |>
        via(method_name, ...) |>
        conclude()
}

clear_predict_key = function(key) {
    if (exists(key, envir = register_predict, inherits = FALSE)) {
        rm(list = key, envir = register_predict)
    }
}

# ---- method_predict() constructor ----

test_that("method_predict() accepts a default function with no variants", {
    mp = method_predict(default = function(.x, new_data = NULL, ...) NULL)

    expect_true(is.function(mp@default))
    expect_equal(mp@variants, list())
})

test_that("method_predict() rejects a non-function, non-NULL default", {
    expect_error(
        method_predict(default = "not_a_function"),
        "must be a function or"
    )
})

test_that("method_predict() rejects non-function variants", {
    expect_error(
        method_predict(
            default = function(.x, new_data = NULL, ...) NULL,
            alt = "not_a_function"
        ),
        "must be functions"
    )
})

# ---- making_predict() ----

test_that("making_predict() carries the model function and model_type untouched", {
    model_fn = new_fake_predict_model("predict_ut_build")
    call_obj = making_predict(model_fn, S7::class_formula)

    expect_s3_class(call_obj, "making_predict_call")
    expect_identical(call_obj$obj, model_fn)
    expect_identical(call_obj$model_type, S7::class_formula)
})

# ---- %<-% / making_predict_register(): input validation ----

test_that("%<-% rejects a model function not built with MODEL_FN", {
    plain_fn = function() NULL

    expect_error(
        making_predict(plain_fn, S7::class_formula) %<-%
            method_predict(default = function(.x, new_data = NULL, ...) NULL),
        "must be a function built with"
    )
})

test_that("%<-% rejects a model_type that isn't a var_id subclass or a formula", {
    model_fn = new_fake_predict_model("predict_ut_bad_model_type")

    expect_error(
        making_predict(model_fn, numeric) %<-%
            method_predict(default = function(.x, new_data = NULL, ...) NULL),
        "must be a class inheriting from"
    )
})

test_that("%<-% accepts a var_id subclass (not just S7::class_formula) as model_type", {
    model_fn = new_fake_predict_model("predict_ut_xby_model_type")
    on.exit(clear_predict_key("predict_ut_xby_model_type_x_by"))

    expect_no_error(
        making_predict(model_fn, x_by) %<-%
            method_predict(default = function(.x, new_data = NULL, ...) NULL)
    )
})

test_that("%<-% rejects a right-hand side that isn't a method_predict object", {
    model_fn = new_fake_predict_model("predict_ut_bad_rhs")

    expect_error(
        making_predict(model_fn, S7::class_formula) %<-%
            list(default = function(.x, new_data = NULL, ...) NULL),
        "Right-hand side of"
    )
})

# ---- making_predict_register(): fresh registration vs. merge ----

test_that("a later registration on the same key keeps the old default when the new one omits it", {
    model_fn = new_fake_predict_model("predict_ut_merge", with_variant = TRUE)
    on.exit(clear_predict_key("predict_ut_merge_formula"))

    making_predict(model_fn, S7::class_formula) %<-%
        method_predict(
            default = function(.x, new_data = NULL, ...) {
                tibble::tibble(.pred = .x@data$coef)
            }
        )
    making_predict(model_fn, S7::class_formula) %<-%
        method_predict(
            alt = function(.x, new_data = NULL, ...) {
                tibble::tibble(.pred = .x@data$coef * 10)
            }
        )

    default_out = predict(fit_default(model_fn))
    alt_out = predict(fit_variant(model_fn, "alt"))

    expect_equal(default_out$.pred, 1)
    expect_equal(alt_out$.pred, 20)
})

test_that("a later registration on the same key overwrites the default when a new one is supplied", {
    model_fn = new_fake_predict_model(
        "predict_ut_overwrite",
        with_variant = TRUE
    )
    on.exit(clear_predict_key("predict_ut_overwrite_formula"))

    making_predict(model_fn, S7::class_formula) %<-%
        method_predict(
            alt = function(.x, new_data = NULL, ...) {
                tibble::tibble(.pred = .x@data$coef * 10)
            }
        )
    making_predict(model_fn, S7::class_formula) %<-%
        method_predict(
            default = function(.x, new_data = NULL, ...) {
                tibble::tibble(.pred = .x@data$coef * 100)
            }
        )

    default_out = predict(fit_default(model_fn))
    alt_out = predict(fit_variant(model_fn, "alt"))

    expect_equal(default_out$.pred, 100)
    expect_equal(alt_out$.pred, 20)
})

# ---- dispatch_predict() registry branches ----

test_that("predict() resolves a named variant when one is registered", {
    model_fn = new_fake_predict_model(
        "predict_ut_variant_hit",
        with_variant = TRUE
    )
    on.exit(clear_predict_key("predict_ut_variant_hit_formula"))

    making_predict(model_fn, S7::class_formula) %<-%
        method_predict(
            default = function(.x, new_data = NULL, ...) {
                tibble::tibble(.pred = .x@data$coef)
            },
            alt = function(.x, new_data = NULL, ...) {
                tibble::tibble(.pred = .x@data$coef + 1000)
            }
        )

    predict_out = predict(fit_variant(model_fn, "alt"))

    expect_equal(predict_out$.pred, 1002)
})

test_that("predict() aborts when the variant name has no matching entry", {
    model_fn = new_fake_predict_model(
        "predict_ut_variant_miss",
        with_variant = TRUE
    )
    on.exit(clear_predict_key("predict_ut_variant_miss_formula"))

    making_predict(model_fn, S7::class_formula) %<-%
        method_predict(
            default = function(.x, new_data = NULL, ...) {
                tibble::tibble(.pred = .x@data$coef)
            }
        )

    expect_error(
        predict(fit_variant(model_fn, "alt")),
        "No predict entry for variant"
    )
})

test_that("predict() aborts when method is \"default\" but no default function was registered", {
    model_fn = new_fake_predict_model(
        "predict_ut_no_default",
        with_variant = TRUE
    )
    on.exit(clear_predict_key("predict_ut_no_default_formula"))

    making_predict(model_fn, S7::class_formula) %<-%
        method_predict(
            alt = function(.x, new_data = NULL, ...) {
                tibble::tibble(.pred = .x@data$coef)
            }
        )

    expect_error(
        predict(fit_default(model_fn)),
        "predict function registered"
    )
})

test_that("predict() aborts when a registered method returns something other than a data frame", {
    model_fn = new_fake_predict_model("predict_ut_bad_return")
    on.exit(clear_predict_key("predict_ut_bad_return_formula"))

    making_predict(model_fn, S7::class_formula) %<-%
        method_predict(
            default = function(.x, new_data = NULL, ...) {
                list(not_a_data_frame = TRUE)
            }
        )

    expect_error(
        predict(fit_default(model_fn)),
        "must return a"
    )
})

# ---- auto_predict() guard branches ----

test_that("auto_predict() aborts when called directly on a non-class_stat_infer object", {
    expect_error(auto_predict(list(a = 1)), "must inherit")
})

test_that("auto_predict() aborts when no method is implemented for a class_stat_infer subclass", {
    dummy_cls = S7::new_class(
        "predict_ut_no_auto_predict",
        parent = class_stat_infer
    )

    expect_error(auto_predict(dummy_cls()), "method for")
})

# ---- predict() on class_lm_object, no new_data (training data) ----

test_that("predict() with no new_data returns fitted values matching base R lm()", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    predict_out = predict(fit)
    base_fit = lm(dist ~ speed, data = cars)

    expect_s3_class(predict_out, "tbl_df")
    expect_equal(
        predict_out$.pred,
        unname(base_fit$fitted.values),
        tolerance = 1e-8
    )
})

test_that("predict() with no new_data includes a truth column matching the response", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    predict_out = predict(fit)

    expect_true("truth" %in% names(predict_out))
    expect_equal(predict_out$truth, cars$dist)
})

# ---- predict() with new_data ----

test_that("predict() with new_data produces different predictions than training data", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    new_speeds = data.frame(speed = c(10, 20, 30))
    predict_out = predict(fit, new_data = new_speeds)
    base_fit = lm(dist ~ speed, data = cars)

    expect_equal(
        predict_out$.pred,
        unname(predict(base_fit, newdata = new_speeds)),
        tolerance = 1e-8
    )
})

test_that("predict() with new_data omits truth when the response column is absent", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    predict_out = predict(fit, new_data = data.frame(speed = c(10, 20)))

    expect_false("truth" %in% names(predict_out))
})

test_that("predict() with new_data includes truth when the response column is present", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    new_dat = data.frame(speed = c(10, 20), dist = c(30, 55))
    predict_out = predict(fit, new_data = new_dat)

    expect_true("truth" %in% names(predict_out))
    expect_equal(predict_out$truth, new_dat$dist)
})

test_that("predict() accepts a tibble for new_data", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    new_dat = tibble::tibble(speed = c(10, 20))
    predict_out = predict(fit, new_data = new_dat)

    expect_s3_class(predict_out, "tbl_df")
    expect_length(predict_out$.pred, 2L)
})

test_that("predict() errors when new_data is not a data frame", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_error(predict(fit, new_data = matrix(1:4, nrow = 2)))
})

# ---- predict() intervals ----

test_that("predict() with interval = 'confidence' adds lower/upper columns", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    predict_out = predict(fit, interval = "confidence")

    expect_true(all(c(".pred_lower", ".pred_upper") %in% names(predict_out)))
    expect_true(all(predict_out$.pred_lower < predict_out$.pred_upper))
})

test_that("predict() confidence interval matches base R predict.lm()", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    predict_out = predict(fit, interval = "confidence", level = 0.9)
    base_fit = lm(dist ~ speed, data = cars)
    base_ci = predict(base_fit, interval = "confidence", level = 0.9)

    expect_equal(
        predict_out$.pred_lower,
        unname(base_ci[, "lwr"]),
        tolerance = 1e-6
    )
    expect_equal(
        predict_out$.pred_upper,
        unname(base_ci[, "upr"]),
        tolerance = 1e-6
    )
})

test_that("predict() prediction interval matches base R predict.lm()", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    predict_out = predict(fit, interval = "prediction")
    base_fit = lm(dist ~ speed, data = cars)
    base_pi = suppressWarnings(predict(base_fit, interval = "prediction"))

    expect_equal(
        predict_out$.pred_lower,
        unname(base_pi[, "lwr"]),
        tolerance = 1e-6
    )
    expect_equal(
        predict_out$.pred_upper,
        unname(base_pi[, "upr"]),
        tolerance = 1e-6
    )
})

test_that("predict() prediction interval is wider than confidence interval", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    ci_out = predict(fit, interval = "confidence")
    pi_out = predict(fit, interval = "prediction")

    ci_width = ci_out$.pred_upper - ci_out$.pred_lower
    pi_width = pi_out$.pred_upper - pi_out$.pred_lower

    expect_true(all(pi_width > ci_width))
})

test_that("predict() errors on interval = 'prediction' for a non-gaussian family", {
    fake_fit = class_lm_object(
        terms = stats::terms(dist ~ speed),
        fitted = cars$dist,
        residuals = rep(0, nrow(cars)),
        beta = c(`(Intercept)` = 0, speed = 1),
        std_beta = c(`(Intercept)` = 1, speed = 1),
        df_residual = nrow(cars) - 2L,
        deviance = 1,
        dispersion = 1,
        family = "binomial"
    )

    expect_error(
        auto_predict(fake_fit, interval = "prediction"),
        "only valid for Gaussian models"
    )
})

test_that("predict() with categorical predictors matches base R with unseen-safe factor handling", {
    fit = iris |>
        define_model(Sepal.Length ~ Species) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    new_dat = data.frame(
        Species = factor("setosa", levels = levels(iris$Species))
    )
    predict_out = predict(fit, new_data = new_dat)
    base_fit = lm(Sepal.Length ~ Species, data = iris)

    expect_equal(
        predict_out$.pred,
        unname(predict(base_fit, newdata = new_dat)),
        tolerance = 1e-8
    )
})

# ---- predict() dispatch/return-type guarantees ----

test_that("predict() output always inherits data.frame", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_true(inherits(predict(fit), "data.frame"))
})

test_that("predict() with missing new_data and explicit new_data give the same training-data result", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    no_arg = predict(fit)
    explicit_df = predict(fit, new_data = cars)

    expect_equal(no_arg$.pred, explicit_df$.pred, tolerance = 1e-8)
})

# ---- predict() registry escape hatch (making_predict) ----

fake_model_def = model_infer_define(
    model_type = S7::class_formula,
    impl = agendas(
        base = baseline(fn = function(.proc, ...) {
            list(coef = 42, note = "not a class_stat_infer")
        })
    )
)

FAKE_PREDICT_MODEL = MODEL_FN(
    cls = "fake_predict_model",
    defs = list(fake_model_def),
    .name = "Fake Model For Predict Registry Test"
)

test_that("predict() errors clearly when fn returns a non-class_stat_infer object with no registry entry", {
    fit = mtcars |>
        define_model(mpg ~ wt) |>
        prepare_model(FAKE_PREDICT_MODEL) |>
        conclude()

    expect_error(predict(fit), "No predict method found")
})

test_that("predict() dispatches through making_predict once registered", {
    making_predict(FAKE_PREDICT_MODEL, S7::class_formula) %<-%
        method_predict(
            default = function(.x, new_data = NULL, ...) {
                tibble::tibble(.pred = .x@data$coef)
            }
        )

    fit = mtcars |>
        define_model(mpg ~ wt) |>
        prepare_model(FAKE_PREDICT_MODEL) |>
        conclude()

    predict_out = predict(fit)

    expect_s3_class(predict_out, "tbl_df")
    expect_equal(predict_out$.pred, 42)
})
