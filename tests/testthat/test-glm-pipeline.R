test_that("glm_to_glm_object() populates predict-related slots", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    expect_equal(obj@fitted, unname(fitted(fit)), tolerance = 1e-8)
    expect_equal(obj@vcov, vcov(fit), tolerance = 1e-8)
    expect_equal(obj@x_mat, as.numeric(model.matrix(fit)))
    expect_type(obj@x_levels, "list")
})

test_that("predict() on class_glm_object returns a tibble", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    out = auto_predict(obj)

    expect_s3_class(out, "tbl_df")
})

test_that("predict() with new_data = NULL returns one row per training observation", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    out = auto_predict(obj)

    expect_equal(nrow(out), nrow(mtcars))
})

test_that("predict() default type = 'response' matches fitted(fit)", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    out = auto_predict(obj)

    expect_equal(out$.pred, unname(fitted(fit)), tolerance = 1e-8)
})

# test_that("predict() includes truth for training data", {
#     fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
#     obj = glm_to_glm_object(fit)
#
#     out = auto_predict(obj)
#
#     expect_equal(out$truth, mtcars$am)
# })

test_that("predict() type = 'link' matches predict.glm(type = 'link')", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    out = auto_predict(obj, type = "link")
    base = unname(predict(fit, type = "link"))

    expect_equal(out$.pred, base, tolerance = 1e-4)
})

test_that("predict() type = 'response' matches predict.glm(type = 'response')", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    out = auto_predict(obj, type = "response")
    base = unname(predict(fit, type = "response"))

    expect_equal(out$.pred, base, tolerance = 1e-8)
})

test_that("predict() with new_data returns one row per new observation", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    new_data = data.frame(wt = c(2.5, 3.5), hp = c(100, 150))
    out = auto_predict(obj, new_data = new_data)

    expect_equal(nrow(out), 2L)
})

test_that("predict() with new_data matches predict.glm() on new data", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    new_data = data.frame(wt = c(2.5, 3.5), hp = c(100, 150))
    out = auto_predict(obj, new_data = new_data, type = "response")
    base = unname(predict(fit, newdata = new_data, type = "response"))

    expect_equal(out$.pred, base, tolerance = 1e-8)
})

test_that("predict() with new_data omits truth when response column is absent", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    new_data = data.frame(wt = c(2.5, 3.5), hp = c(100, 150))
    out = auto_predict(obj, new_data = new_data)

    expect_false("truth" %in% names(out))
})

test_that("predict() with new_data includes truth when response column is present", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    new_data = mtcars[1:5, c("am", "wt", "hp")]
    out = auto_predict(obj, new_data = new_data)

    expect_equal(out$truth, new_data$am)
})

test_that("predict() correctly handles factor predictors via stored x_levels", {
    df = transform(mtcars, cyl = factor(cyl))
    fit = glm(am ~ cyl + wt, data = df, family = binomial())
    obj = glm_to_glm_object(fit)

    new_data = data.frame(cyl = factor("6", levels = levels(df$cyl)), wt = 3)

    expect_no_error(auto_predict(obj, new_data = new_data))
})

test_that("predict() interval = 'confidence' brackets the point estimate", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    out = auto_predict(obj, interval = "confidence")

    expect_true(all(out$.pred_lower <= out$.pred))
    expect_true(all(out$.pred <= out$.pred_upper))
})

test_that("predict() confidence interval on link scale matches manual delta-method SE", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    out = auto_predict(
        obj,
        type = "link",
        interval = "confidence",
        level = 0.95
    )
    base = predict(fit, type = "link", se.fit = TRUE)
    crit = qnorm(0.975)

    expect_equal(
        out$.pred_lower,
        unname(base$fit - crit * base$se.fit),
        tolerance = 1e-6
    )
    expect_equal(
        out$.pred_upper,
        unname(base$fit + crit * base$se.fit),
        tolerance = 1e-6
    )
})

test_that("predict() confidence interval uses normal critical value for binomial family", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    out = auto_predict(obj, type = "link", interval = "confidence", level = 0.9)
    base = predict(fit, type = "link", se.fit = TRUE)
    crit = qnorm(0.95)

    expect_equal(
        out$.pred_lower,
        unname(base$fit - crit * base$se.fit),
        tolerance = 1e-6
    )
})

test_that("predict() confidence interval uses t critical value for gaussian family", {
    fit = glm(mpg ~ wt + hp, data = mtcars, family = gaussian())
    obj = glm_to_glm_object(fit)

    out = auto_predict(
        obj,
        type = "link",
        interval = "confidence",
        level = 0.95
    )
    base = predict(fit, type = "link", se.fit = TRUE)
    crit = qt(0.975, df = fit$df.residual)

    expect_equal(
        out$.pred_lower,
        unname(base$fit - crit * base$se.fit),
        tolerance = 1e-6
    )
})

test_that("predict() confidence interval uses normal critical value for poisson family", {
    fit = glm(carb ~ wt + hp, data = mtcars, family = poisson())
    obj = glm_to_glm_object(fit)

    out = auto_predict(
        obj,
        type = "link",
        interval = "confidence",
        level = 0.95
    )
    base = predict(fit, type = "link", se.fit = TRUE)
    crit = qnorm(0.975)

    expect_equal(
        out$.pred_lower,
        unname(base$fit - crit * base$se.fit),
        tolerance = 1e-6
    )
})

test_that("predict() confidence interval on response scale is asymmetric around .pred", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    out = auto_predict(obj, type = "response", interval = "confidence")

    expect_false(isTRUE(all.equal(
        out$.pred - out$.pred_lower,
        out$.pred_upper - out$.pred
    )))
})

test_that("predict() errors on invalid type argument", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    expect_error(auto_predict(obj, type = "not_a_type"), class = "rlang_error")
})

test_that("predict() errors on invalid interval argument", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    expect_error(
        auto_predict(obj, interval = "prediction"),
        class = "rlang_error"
    )
})

test_that("predict() works end-to-end through the GLM pipeline", {
    result = mtcars |>
        define_model(am ~ wt + hp) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    out = predict(result)

    expect_s3_class(out, "data.frame")
    expect_named(out, c("truth", ".pred"))
})

test_that("predict() on GLM pipeline result matches auto_predict() on cld_exec@data", {
    result = mtcars |>
        define_model(am ~ wt + hp) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    via_predict = predict(result)
    via_auto = auto_predict(result@data)

    expect_equal(via_predict, via_auto)
})
