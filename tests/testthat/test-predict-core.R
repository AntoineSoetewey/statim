# ---- predict() on class_lm_object, no new_data (training data) ----

test_that("predict() with no new_data returns fitted values matching base R lm()", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    predict_out = predict(fit)
    base_fit = lm(dist ~ speed, data = cars)

    expect_s3_class(predict_out, "tbl_df")
    expect_equal(predict_out$.pred, unname(base_fit$fitted.values), tolerance = 1e-8)
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

    expect_equal(predict_out$.pred_lower, unname(base_ci[, "lwr"]), tolerance = 1e-6)
    expect_equal(predict_out$.pred_upper, unname(base_ci[, "upr"]), tolerance = 1e-6)
})

test_that("predict() prediction interval matches base R predict.lm()", {
    fit = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    predict_out = predict(fit, interval = "prediction")
    base_fit = lm(dist ~ speed, data = cars)
    base_pi = suppressWarnings(predict(base_fit, interval = "prediction"))

    expect_equal(predict_out$.pred_lower, unname(base_pi[, "lwr"]), tolerance = 1e-6)
    expect_equal(predict_out$.pred_upper, unname(base_pi[, "upr"]), tolerance = 1e-6)
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

    new_dat = data.frame(Species = factor("setosa", levels = levels(iris$Species)))
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
    making_predict(FAKE_PREDICT_MODEL, S7::class_formula) %<-% method_predict(
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
