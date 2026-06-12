test_that("glm_to_glm_object() returns a class_glm_object from a fitted glm", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    expect_s7_class(obj, statim::class_glm_object)
})

test_that("glm_to_glm_object() errors on non-glm input", {
    expect_error(
        glm_to_glm_object(list(x = 1)),
        class = "rlang_error"
    )
})

test_that("glm_to_glm_object() coefficients df has correct columns", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    expect_named(obj@coefficients, c("term", "estimate", "std_error", "statistic", "p_value"))
})

test_that("glm_to_glm_object() fit_summary has correct columns", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    expect_named(
        obj@fit_summary,
        c("family", "link", "null_deviance", "deviance", "df_residual", "aic", "n_obs")
    )
})

test_that("glm_to_glm_object() dispersion is 1 for binomial family", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    expect_equal(obj@dispersion, 1)
})

test_that("glm_to_glm_object() dispersion matches summary() for gaussian family", {
    fit = glm(mpg ~ wt + hp, data = mtcars, family = gaussian())
    obj = glm_to_glm_object(fit)

    expect_equal(obj@dispersion, summary(fit)$dispersion, tolerance = 1e-8)
})

test_that("glm_to_glm_object() deviance matches fitted glm deviance", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    expect_equal(obj@deviance, fit$deviance, tolerance = 1e-8)
})

test_that("GLM() eager via formula returns stat_infer_spec", {
    result = GLM(am ~ wt + hp, mtcars)

    expect_s7_class(result, stat_infer_spec)
})

test_that("GLM() eager result data is a class_glm_object", {
    result = GLM(am ~ wt + hp, mtcars)

    expect_s7_class(result@data, statim::class_glm_object)
})

test_that("GLM() default family is gaussian when omitted", {
    result = GLM(mpg ~ wt + hp, mtcars)
    base = glm(mpg ~ wt + hp, data = mtcars, family = gaussian())

    expect_equal(
        result@data@coefficients$estimate,
        unname(coef(base)),
        tolerance = 1e-8
    )
})

test_that("GLM() with binomial family matches base glm() coefficients", {
    result = mtcars |>
        define_model(am ~ wt + hp) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    base = glm(am ~ wt + hp, data = mtcars, family = binomial())

    expect_equal(
        result@data@coefficients$estimate,
        unname(coef(base)),
        tolerance = 1e-8
    )
})

test_that("GLM() pipeline returns cld_exec", {
    result = mtcars |>
        define_model(am ~ wt + hp) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("GLM() pipeline `stat_name` is 'Generalized Linear Model'", {
    result = mtcars |>
        define_model(am ~ wt + hp) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    expect_equal(result@cld_meta$stat_name, "Generalized Linear Model")
})

test_that("GLM() pipeline print returns invisibly", {
    result = mtcars |>
        define_model(am ~ wt + hp) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    expect_invisible(print(result))
})

test_that("GLM() with poisson family runs without error", {
    expect_no_error(
        mtcars |>
            define_model(carb ~ wt + hp) |>
            prepare_model(GLM) |>
            update(family = poisson()) |>
            conclude()
    )
})

test_that("GLM() pipeline coefficients match formula-only result for gaussian", {
    pipeline_result = mtcars |>
        define_model(mpg ~ wt + hp) |>
        prepare_model(GLM) |>
        conclude()

    eager_result = GLM(mpg ~ wt + hp, mtcars)

    expect_equal(
        pipeline_result@data@coefficients$estimate,
        eager_result@data@coefficients$estimate,
        tolerance = 1e-8
    )
})

test_that("class_glm_object inherits from anova_able", {
    fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
    obj = glm_to_glm_object(fit)

    expect_true(S7::S7_inherits(obj, anova_able))
})
