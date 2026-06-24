pipeline_conclude = function(data, formula) {
    data |>
        define_model(formula) |>
        prepare_model(LINEAR_REG) |>
        conclude()
}

pipeline_lazy = function(data, formula) {
    data |>
        define_model(formula) |>
        prepare_model(LINEAR_REG)
}

is_cld_anova = function(x) S7::S7_inherits(x, cld_anova)

test_that("anova() F->LRT switch is reflected in cld_meta$method", {
    mod1 = mtcars |>
        define_model(am ~ wt) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    mod2 = mtcars |>
        define_model(am ~ wt + hp) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    result = suppressMessages(anova(mod1, mod2, test = "F"))

    expect_equal(result@cld_meta$method, "LRT")
})

test_that("print(cld_anova) after F->LRT switch shows LRT not F in header", {
    mod1 = mtcars |>
        define_model(am ~ wt) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    mod2 = mtcars |>
        define_model(am ~ wt + hp) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    result = suppressMessages(anova(mod1, mod2, test = "F"))
    out = paste(capture.output(print(result)), collapse = "\n")

    expect_true(grepl("LRT", out))
    expect_false(grepl("ANOVA \u00b7 F", out))
})

# ---- build_anova() family mismatch guard ----

test_that("anova() errors when models have different families", {
    gaussian_mod = mtcars |>
        define_model(am ~ wt) |>
        prepare_model(GLM) |>
        conclude()

    binomial_mod = mtcars |>
        define_model(am ~ wt) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    expect_error(
        anova(gaussian_mod, binomial_mod),
        class = "rlang_error"
    )
    expect_error(
        anova(gaussian_mod, binomial_mod),
        regexp = "same error family"
    )
})

# ---- build_anova() nobs mismatch guard ----

test_that("anova() errors when models are fitted on different numbers of observations", {
    mod1 = mtcars |>
        define_model(mpg ~ wt) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    mod2 = mtcars[-1L, ] |>
        define_model(mpg ~ wt) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_error(
        anova(mod1, mod2),
        class = "rlang_error"
    )
    expect_error(
        anova(mod1, mod2),
        regexp = "same number of observations"
    )
})

# ---- print(anova_lazy): Args line ----

test_that("print(anova_lazy) shows Args line when recalibrate_spec has args", {
    mod1 = pipeline_lazy(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_lazy(LifeCycleSavings, sr ~ pop15)

    mod1_recal = stats::update(mod1, x = TRUE)
    obj = anova_lazy(models = list(mod1_recal, mod2), labels = c("f1", "f2"))

    out = paste(capture.output(print(obj)), collapse = "\n")

    expect_true(grepl("Args", out))
})

test_that("update() on multi_lazy applies args to all model_lazy models", {
    ml = mtcars |>
        write_models(
            null = am ~ 1,
            main = am ~ wt
        ) |>
        prepare_model(GLM)

    updated = stats::update(ml, family = binomial())

    expect_s7_class(updated, multi_lazy)
    expect_length(updated@models, 2L)
})

test_that("update() on multi_lazy feeds correctly into anova()", {
    result = mtcars |>
        write_models(
            null = am ~ 1,
            main = am ~ wt
        ) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        anova(test = "LRT")

    expect_true(is_cld_anova(result))
    expect_equal(result@data$model, c("null", "main"))
    expect_true("chisq_value" %in% names(result@data))
})

test_that("update() on multi_lazy second call takes recalibrate_spec branch", {
    ml = mtcars |>
        write_models(
            null = am ~ 1,
            main = am ~ wt
        ) |>
        prepare_model(GLM)

    once = stats::update(ml, family = binomial())
    twice = stats::update(once, family = binomial())

    expect_s7_class(twice, multi_lazy)
    expect_length(twice@models, 2L)
})

# ---- write_models(): rel() var_id ----

test_that("write_models() accepts rel() as var_id", {
    em = mtcars |>
        write_models(
            m1 = rel(wt, mpg),
            m2 = rel(hp, mpg)
        )

    expect_s7_class(em, expanded_model)
    expect_length(em@models, 2L)
    expect_equal(em@labels, c("m1", "m2"))
})

test_that("write_models() with rel() feeds correctly into prepare_model()", {
    ml = mtcars |>
        write_models(
            m1 = rel(wt, mpg),
            m2 = rel(hp, mpg)
        ) |>
        prepare_model(LINEAR_REG)

    expect_s7_class(ml, multi_lazy)
    expect_length(ml@models, 2L)
})

test_that("write_models() with rel() conclude() returns multi_exec", {
    me = mtcars |>
        write_models(
            m1 = rel(wt, mpg),
            m2 = rel(hp, mpg)
        ) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_s7_class(me, multi_exec)
    expect_length(me@results, 2L)
    expect_equal(me@labels, c("m1", "m2"))
})

# ---- write_models(): mixed var_id types ----

test_that("write_models() accepts mixed formula and rel() in one call", {
    em = mtcars |>
        write_models(
            null = mpg ~ 1,
            m1 = rel(wt, mpg),
            m2 = rel(hp, mpg)
        )

    expect_s7_class(em, expanded_model)
    expect_length(em@models, 3L)
    expect_equal(em@labels, c("null", "m1", "m2"))
})

test_that("write_models() mixed var_id types feed correctly into conclude()", {
    me = mtcars |>
        write_models(
            null = mpg ~ 1,
            m1 = rel(wt, mpg),
            m2 = rel(hp, mpg)
        ) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_s7_class(me, multi_exec)
    expect_length(me@results, 3L)
    expect_equal(me@labels, c("null", "m1", "m2"))
})

test_that("write_models() mixed var_id types each result is a cld_exec", {
    me = mtcars |>
        write_models(
            null = mpg ~ 1,
            m1 = rel(wt, mpg),
            m2 = rel(hp, mpg)
        ) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_true(S7::S7_inherits(me@results[[1L]], cld_exec))
    expect_true(S7::S7_inherits(me@results[[2L]], cld_exec))
    expect_true(S7::S7_inherits(me@results[[3L]], cld_exec))
})

# ---- write_models(): prepare_test() path ----

test_that("write_models() feeds correctly into prepare_test()", {
    ml = mtcars |>
        write_models(
            by_am = x_by(mpg, am),
            by_vs = x_by(mpg, vs)
        ) |>
        prepare_test(TTEST)

    expect_s7_class(ml, multi_lazy)
    expect_length(ml@models, 2L)
    expect_equal(ml@labels, c("by_am", "by_vs"))
})

test_that("write_models() with prepare_test() conclude() returns multi_exec", {
    me = mtcars |>
        write_models(
            by_am = x_by(mpg, am),
            by_vs = x_by(mpg, vs)
        ) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s7_class(me, multi_exec)
    expect_length(me@results, 2L)
    expect_equal(me@labels, c("by_am", "by_vs"))
})

test_that("write_models() with prepare_test() each result is a cld_exec", {
    me = mtcars |>
        write_models(
            by_am = x_by(mpg, am),
            by_vs = x_by(mpg, vs)
        ) |>
        prepare_test(TTEST) |>
        conclude()

    expect_true(S7::S7_inherits(me@results[[1L]], cld_exec))
    expect_true(S7::S7_inherits(me@results[[2L]], cld_exec))
})
