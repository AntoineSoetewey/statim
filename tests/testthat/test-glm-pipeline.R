test_that("statistic property computes beta / std_beta", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    expect_equal(obj@statistic, obj@beta / obj@std_beta)
})

test_that("p_value property uses a normal reference for binomial family", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    expected = 2 * pnorm(abs(obj@statistic), lower.tail = FALSE)

    expect_equal(obj@p_value, expected)
})

test_that("p_value property uses a normal reference for poisson family", {
    fit = glm(carb ~ wt + hp, data = mtcars, family = poisson())
    obj = glm_to_glm_object(fit)

    expected = 2 * pnorm(abs(obj@statistic), lower.tail = FALSE)

    expect_equal(obj@p_value, expected)
})

test_that("p_value property uses a t reference for gaussian family", {
    fit = glm(mpg ~ wt + hp, data = mtcars, family = gaussian())
    obj = glm_to_glm_object(fit)

    expected = 2 *
        pt(abs(obj@statistic), df = obj@df_residual, lower.tail = FALSE)

    expect_equal(obj@p_value, expected)
})

test_that("coefficients property returns the expected tibble shape and values", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    out = obj@coefficients

    expect_s3_class(out, "tbl_df")
    expect_named(
        out,
        c("term", "estimate", "std_error", "statistic", "p_value")
    )
    expect_equal(nrow(out), length(obj@beta))
    expect_equal(out$term, names(obj@beta))
    expect_equal(out$estimate, unname(obj@beta))
    expect_equal(out$std_error, unname(obj@std_beta))
    expect_equal(out$statistic, unname(obj@statistic))
    expect_equal(out$p_value, unname(obj@p_value))
})

test_that("fit_summary property returns the expected tibble shape and values", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    out = obj@fit_summary

    expect_s3_class(out, "tbl_df")
    expect_named(
        out,
        c(
            "family",
            "link",
            "null_deviance",
            "deviance",
            "df_residual",
            "aic",
            "n_obs"
        )
    )
    expect_equal(out$family, obj@family)
    expect_equal(out$link, obj@link)
    expect_equal(out$null_deviance, obj@null_deviance)
    expect_equal(out$deviance, obj@deviance)
    expect_equal(out$df_residual, as.integer(obj@df_residual))
    expect_equal(out$aic, obj@aic)
    expect_equal(out$n_obs, nrow(mtcars))
})

test_that("print() on class_glm_object prints the Coefficients and Model Fit sections", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    expect_output(print(obj), "Coefficients")
    expect_output(print(obj), "Model Fit")
})

test_that("print() on class_glm_object returns its input invisibly", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    out = withVisible(print(obj))

    expect_false(out$visible)
    expect_identical(out$value, obj)
})

test_that("glm_to_glm_object() errors when given a non-glm object", {
    fit = lm(mpg ~ wt, data = mtcars)

    expect_error(glm_to_glm_object(fit), class = "rlang_error")
})

test_that("glm_to_glm_object() error message names the class it actually got", {
    fit = lm(mpg ~ wt, data = mtcars)

    expect_error(glm_to_glm_object(fit), regexp = "lm", class = "rlang_error")
})

test_that("n_params() on class_glm_object returns the number of coefficients", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    expect_equal(n_params(obj), length(obj@beta))
    expect_equal(n_params(obj), 3L)
})
