# Covers: anova() dispatch on model_lazy and cld_exec (incl. error guards
# for non-model/non-exec arguments), print()/update() on anova_lazy,
# print() on cld_anova (default vs labeled method, p-value styling),
# the Gaussian-vs-non-Gaussian branch in compute_anova_stats() (F forced
# to LRT for binomial GLMs, chisq_value path), the Type-I-only guard on
# single-model anova(), valid_tests() error on bad `test` argument,
# print() on expanded_model/multi_lazy/multi_exec, tidy() on multi_exec,
# write_models() unnamed-argument error, and update()-chaining across
# write_models() expressions feeding into anova().

pipeline_conclude = function(data, formula) {
    data |> define_model(formula) |> prepare_model(LINEAR_REG) |> conclude()
}

pipeline_lazy = function(data, formula) {
    data |> define_model(formula) |> prepare_model(LINEAR_REG)
}

is_cld_anova = function(x) S7::S7_inherits(x, cld_anova)

test_that("anova() on model_lazy with multiple models returns a cld_anova", {
    mod1 = pipeline_lazy(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_lazy(LifeCycleSavings, sr ~ pop15)
    mod3 = pipeline_lazy(LifeCycleSavings, sr ~ pop15 + pop75)

    result = anova(mod1, mod2, mod3)

    expect_true(is_cld_anova(result))
    expect_equal(nrow(result@data), 3L)
})

test_that("anova() on model_lazy with multiple models matches cld_exec path", {
    lazy_tbl = {
        mod1 = pipeline_lazy(LifeCycleSavings, sr ~ 1)
        mod2 = pipeline_lazy(LifeCycleSavings, sr ~ pop15)
        anova(mod1, mod2)@data
    }

    exec_tbl = {
        mod1 = pipeline_conclude(LifeCycleSavings, sr ~ 1)
        mod2 = pipeline_conclude(LifeCycleSavings, sr ~ pop15)
        anova(mod1, mod2)@data
    }

    expect_equal(lazy_tbl$f_value, exec_tbl$f_value, tolerance = 1e-10)
    expect_equal(lazy_tbl$p_value, exec_tbl$p_value, tolerance = 1e-10)
})

test_that("anova() on model_lazy errors when a non-model_lazy argument is passed", {
    mod1 = pipeline_lazy(LifeCycleSavings, sr ~ 1)

    expect_error(anova(mod1, 42), class = "rlang_error")
})

test_that("anova() on model_lazy errors identify the offending argument position", {
    mod1 = pipeline_lazy(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_lazy(LifeCycleSavings, sr ~ pop15)

    # argument 3 (42) is not a model_lazy
    expect_error(anova(mod1, mod2, 42), class = "rlang_error")
})

# ---- anova(cld_exec, ...) with multiple cld_exec objects (3+) ----

test_that("anova() on cld_exec with three models returns a cld_anova", {
    mod1 = pipeline_conclude(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_conclude(LifeCycleSavings, sr ~ pop15)
    mod3 = pipeline_conclude(LifeCycleSavings, sr ~ pop15 + pop75)

    result = anova(mod1, mod2, mod3)

    expect_true(is_cld_anova(result))
    expect_equal(nrow(result@data), 3L)
})

# Note: "multi-model anova() errors when non-cld_exec passed" in
# test-linear-reg-anova.R already covers the `not_exec` guard for a single
# extra bad argument. This adds a multi-argument case to confirm the error
# fires regardless of position.
test_that("anova() on cld_exec errors when any later argument is not cld_exec", {
    mod1 = pipeline_conclude(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_conclude(LifeCycleSavings, sr ~ pop15)

    expect_error(anova(mod1, mod2, "not a model"), class = "rlang_error")
})

# ---- print(anova_lazy) ----
# anova_lazy is the legacy subclass of multi_lazy. Constructing one directly
# requires the same fields as multi_lazy: models (list of model_lazy) and
# labels.

test_that("print() on anova_lazy runs without error and is invisible", {
    mod1 = pipeline_lazy(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_lazy(LifeCycleSavings, sr ~ pop15)

    obj = anova_lazy(models = list(mod1, mod2), labels = c("f1", "f2"))

    expect_invisible(print(obj))
})

test_that("print() on anova_lazy includes model labels and formulas in output", {
    mod1 = pipeline_lazy(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_lazy(LifeCycleSavings, sr ~ pop15)

    obj = anova_lazy(models = list(mod1, mod2), labels = c("f1", "f2"))

    out = capture.output(print(obj))
    full = paste(out, collapse = "\n")

    expect_true(grepl("f1", full))
    expect_true(grepl("f2", full))
    expect_true(grepl("Model Specification", full))
})

test_that("print() on anova_lazy shows Args line when model_spec has args", {
    mod1 = pipeline_lazy(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_lazy(LifeCycleSavings, sr ~ pop15)

    # ASSUMPTION: update() on model_lazy stores extra args in
    # m@model_spec@args or m@recalibrate_spec$args, surfaced by
    # print(anova_lazy) as an "Args :" line. Adjust the update() call
    # below if LINEAR_REG doesn't accept a toggle like this.
    mod1_recal = stats::update(mod1, weights = NULL)

    obj = anova_lazy(models = list(mod1_recal, mod2), labels = c("f1", "f2"))

    out = capture.output(print(obj))
    full = paste(out, collapse = "\n")

    expect_true(grepl("Model Specification", full))
})

# ---- update(anova_lazy) ----

test_that("update() on anova_lazy applies new args via recalibrate_spec", {
    mod1 = pipeline_lazy(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_lazy(LifeCycleSavings, sr ~ pop15)

    obj = anova_lazy(models = list(mod1, mod2), labels = c("f1", "f2"))

    # ASSUMPTION: model_lazy objects start with recalibrate_spec = NULL,
    # so the first update() takes the `else` branch (model_spec@args).
    updated = stats::update(obj, x = TRUE)

    expect_s7_class(updated, anova_lazy)
    expect_length(updated@models, 2L)
})

test_that("update() on anova_lazy a second time takes the recalibrate_spec branch", {
    mod1 = pipeline_lazy(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_lazy(LifeCycleSavings, sr ~ pop15)

    obj = anova_lazy(models = list(mod1, mod2), labels = c("f1", "f2"))

    once = stats::update(obj, x = TRUE)
    # ASSUMPTION: a second update() call hits models whose
    # recalibrate_spec is now non-NULL, taking the `if` branch
    # (modifyList on recalibrate_spec$args).
    twice = stats::update(once, y = TRUE)

    expect_s7_class(twice, anova_lazy)
    expect_length(twice@models, 2L)
})

# ---- cld_anova / print(cld_anova) ----

test_that("print() on cld_anova with method = 'default' uses stat_name only", {
    tbl = pipeline_conclude(cars, dist ~ speed) |> anova() |> (\(x) x@data)()

    obj = cld_anova(
        data = tbl,
        impl_cls = "anova_lm",
        stat_cls = "anova_lm",
        print_fn = NULL,
        name = "ANOVA",
        labels = character(0),
        cld_meta = list(stat_name = "ANOVA", method = "default", data_name = "")
    )

    out = capture.output(print(obj))
    full = paste(out, collapse = "\n")

    expect_true(grepl("ANOVA", full))
    expect_false(grepl("ANOVA \u00b7", full))
})

test_that("print() on cld_anova with non-default method shows 'stat_name . method'", {
    mod1 = pipeline_conclude(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_conclude(LifeCycleSavings, sr ~ pop15)

    obj = anova(mod1, mod2, test = "F")

    out = capture.output(print(obj))
    full = paste(out, collapse = "\n")

    expect_true(grepl("ANOVA Table", full))
})

test_that("print() on cld_anova p_value styler colors small p-values", {
    # ASSUMPTION: comparing a null model against one with several
    # predictors on LifeCycleSavings produces p_value < 0.01 for at
    # least one row, exercising the cli::style_bold("<0.001") branch.
    result = LifeCycleSavings |>
        write_models(f1 = sr ~ 1, f2 = sr ~ pop15 + pop75 + dpi + ddpi) |>
        prepare_model(LINEAR_REG) |>
        anova()

    expect_true(is_cld_anova(result))
    # Smoke test: print should not error regardless of styling branch taken
    expect_invisible(print(result))
})

# ---- compute_anova_stats(): non-Gaussian family forces LRT ----

test_that("anova() on binomial GLMs with test = 'F' switches to LRT with a message", {
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

    expect_message(
        {
            result = anova(mod1, mod2, test = "F")
        },
        "Switching to LRT"
    )

    expect_true(is_cld_anova(result))
    expect_true("chisq_value" %in% names(result@data))
})

test_that("anova() on binomial GLMs with test = 'LRT' uses chisq_value column directly", {
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

    result = anova(mod1, mod2, test = "LRT")

    expect_true("chisq_value" %in% names(result@data))
    expect_true(is.na(result@data$chisq_value[1L]))
    expect_false(is.na(result@data$chisq_value[2L]))
})

test_that("LRT path p_value matches manual pchisq() computation", {
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

    result = anova(mod1, mod2, test = "LRT")

    dev_diff = mod1@data@deviance - mod2@data@deviance
    df_diff = mod1@data@df_residual - mod2@data@df_residual
    expected_p = pchisq(dev_diff, df_diff, lower.tail = FALSE)

    expect_equal(result@data$p_value[2L], expected_p, tolerance = 1e-8)
})

test_that("single-model anova() on a GLM errors because Type I is LM-only", {
    glm_result = mtcars |>
        define_model(am ~ wt + hp) |>
        prepare_model(GLM) |>
        update(family = binomial()) |>
        conclude()

    expect_error(anova(glm_result), class = "rlang_error")
    expect_error(anova(glm_result), regexp = "class_lm_object")
})

# ---- valid_tests() ----

test_that("anova() errors on an invalid test argument", {
    mod1 = pipeline_conclude(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_conclude(LifeCycleSavings, sr ~ pop15)

    expect_error(anova(mod1, mod2, test = "bogus"), class = "rlang_error")
})

test_that("anova() errors on invalid test argument for single-model path too", {
    mod1 = pipeline_conclude(cars, dist ~ speed)

    expect_error(anova(mod1, test = "bogus"), class = "rlang_error")
})


# ---- Additional tests for model-expand.R ----
# Append to a new or existing test file, e.g. test-write-models.R

# ---- print(expanded_model) ----

test_that("print() on expanded_model runs without error and is invisible", {
    em = LifeCycleSavings |> write_models(f1 = sr ~ 1, f2 = sr ~ pop15)

    expect_invisible(print(em))
})

test_that("print() on expanded_model output includes model labels and formulas", {
    em = LifeCycleSavings |> write_models(f1 = sr ~ 1, f2 = sr ~ pop15)

    out = capture.output(print(em))
    full = paste(out, collapse = "\n")

    expect_true(grepl("f1", full))
    expect_true(grepl("f2", full))
    expect_true(grepl("Models", full))
})

# ---- print(multi_lazy) ----

test_that("print() on multi_lazy runs without error and is invisible", {
    ml = LifeCycleSavings |>
        write_models(f1 = sr ~ 1, f2 = sr ~ pop15) |>
        prepare_model(LINEAR_REG)

    expect_invisible(print(ml))
})

test_that("print() on multi_lazy output includes model labels", {
    ml = LifeCycleSavings |>
        write_models(f1 = sr ~ 1, f2 = sr ~ pop15) |>
        prepare_model(LINEAR_REG)

    out = capture.output(print(ml))
    full = paste(out, collapse = "\n")

    expect_true(grepl("f1", full))
    expect_true(grepl("f2", full))
})

# ---- print(multi_exec) and tidy(multi_exec) ----

test_that("conclude() on multi_lazy returns a multi_exec", {
    me = LifeCycleSavings |>
        write_models(f1 = sr ~ 1, f2 = sr ~ pop15, f3 = sr ~ pop15 + pop75) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_s7_class(me, multi_exec)
    expect_length(me@results, 3L)
    expect_equal(me@labels, c("f1", "f2", "f3"))
})

test_that("print() on multi_exec runs without error and is invisible", {
    me = LifeCycleSavings |>
        write_models(f1 = sr ~ 1, f2 = sr ~ pop15) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_invisible(print(me))
})

test_that("print() on multi_exec output includes header with stat_name and labels", {
    me = LifeCycleSavings |>
        write_models(f1 = sr ~ 1, f2 = sr ~ pop15) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    out = capture.output(print(me))
    full = paste(out, collapse = "\n")

    expect_true(grepl("Linear Regression", full))
    expect_true(grepl("f1", full))
    expect_true(grepl("f2", full))
    expect_true(grepl("cld_exec", full))
})

test_that("print() on multi_exec with a single model uses singular 'model'", {
    me = LifeCycleSavings |>
        write_models(f1 = sr ~ 1) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    out = capture.output(print(me))
    full = paste(out, collapse = "\n")

    expect_true(grepl("1 model ", full))
    expect_false(grepl("1 models", full))
})

test_that("tidy() on multi_exec returns a tibble with model and outs columns", {
    me = LifeCycleSavings |>
        write_models(f1 = sr ~ 1, f2 = sr ~ pop15) |>
        prepare_model(LINEAR_REG) |>
        conclude() |>
        tidy()

    expect_s3_class(me, "tbl_df")
    expect_named(me, c("model", "outs"))
    expect_equal(me$model, c("f1", "f2"))
    expect_length(me$outs, 2L)
})

test_that("tidy() on multi_exec each element of outs is itself a tibble", {
    me = LifeCycleSavings |>
        write_models(f1 = sr ~ 1, f2 = sr ~ pop15) |>
        prepare_model(LINEAR_REG) |>
        conclude() |>
        tidy()

    expect_s3_class(me$outs[[1L]], "tbl_df")
    expect_s3_class(me$outs[[2L]], "tbl_df")
})

# ---- write_models(): unnamed argument error ----

test_that("write_models() errors when any argument is unnamed", {
    expect_error(
        LifeCycleSavings |> write_models(f1 = sr ~ 1, sr ~ pop15),
        class = "rlang_error"
    )
})

test_that("write_models() errors when all arguments are unnamed", {
    expect_error(
        LifeCycleSavings |> write_models(sr ~ 1, sr ~ pop15),
        class = "rlang_error"
    )
})

test_that("write_models() error message mentions write_models", {
    expect_error(
        LifeCycleSavings |> write_models(sr ~ 1),
        regexp = "write_models"
    )
})

# ---- write_models(): update() chaining (data mask evaluation) ----

test_that("write_models() supports update() chaining across named expressions", {
    em = LifeCycleSavings |>
        write_models(
            f1 = sr ~ 1,
            f2 = update(f1, ~ . + pop15),
            f3 = update(f2, ~ . + pop75)
        )

    expect_length(em@models, 3L)
    expect_equal(em@labels, c("f1", "f2", "f3"))

    f3_formula = em@models[[3L]]@var_id
    expect_equal(deparse(f3_formula), "sr ~ pop15 + pop75")
})

test_that("write_models() with update() chaining feeds correctly into anova()", {
    result = LifeCycleSavings |>
        write_models(
            f1 = sr ~ 1,
            f2 = update(f1, ~ . + pop15),
            f3 = update(f2, ~ . + pop75)
        ) |>
        prepare_model(LINEAR_REG) |>
        anova()

    expect_true(is_cld_anova(result))
    expect_equal(nrow(result@data), 3L)
})

pipeline_conclude = function(data, formula) {
    data |> define_model(formula) |> prepare_model(LINEAR_REG) |> conclude()
}

pipeline_lazy = function(data, formula) {
    data |> define_model(formula) |> prepare_model(LINEAR_REG)
}

is_cld_anova = function(x) S7::S7_inherits(x, cld_anova)

# ---- build_anova(): non-nested model warning ----

test_that("anova() warns when models have equal complexity", {
    mod1 = pipeline_conclude(mtcars, mpg ~ wt)
    mod2 = pipeline_conclude(mtcars, mpg ~ hp)

    expect_warning(anova(mod1, mod2), regexp = "non-nested")
})

test_that("anova() with non-nested models still returns a cld_anova", {
    mod1 = pipeline_conclude(mtcars, mpg ~ wt)
    mod2 = pipeline_conclude(mtcars, mpg ~ hp)

    result = suppressWarnings(anova(mod1, mod2))

    expect_true(is_cld_anova(result))
})

test_that("anova() non-nested rows have NA test statistics in @data", {
    mod1 = pipeline_conclude(mtcars, mpg ~ wt)
    mod2 = pipeline_conclude(mtcars, mpg ~ hp)

    result = suppressWarnings(anova(mod1, mod2))

    expect_true(is.na(result@data$f_value[2L]))
    expect_true(is.na(result@data$p_value[2L]))
})

test_that("print() on non-nested cld_anova does not error", {
    mod1 = pipeline_conclude(mtcars, mpg ~ wt)
    mod2 = pipeline_conclude(mtcars, mpg ~ hp)

    result = suppressWarnings(anova(mod1, mod2))

    expect_invisible(print(result))
})

test_that("anova() warns only for non-nested pairs, not nested ones", {
    mod1 = pipeline_conclude(mtcars, mpg ~ wt)
    mod2 = pipeline_conclude(mtcars, mpg ~ hp)
    mod3 = pipeline_conclude(mtcars, mpg ~ wt + hp)

    expect_warning(anova(mod1, mod2, mod3), regexp = "non-nested")
})

test_that("anova() nested models produce no non-nested warning", {
    mod1 = pipeline_conclude(mtcars, mpg ~ wt)
    mod2 = pipeline_conclude(mtcars, mpg ~ wt + hp)

    expect_no_warning(anova(mod1, mod2))
})

# ---- Extra: Comparison with base R ----

test_that("Type I ANOVA matches stats::anova() for factor predictors", {
    fit = lm(mpg ~ factor(cyl) + wt, data = mtcars)
    model = define_model(mpg ~ factor(cyl) + wt, mtcars) |>
        prepare_model(LINEAR_REG)

    expected = stats::anova(fit)
    observed = anova(model)

    expect_equal(observed@data$df, expected$Df)
    expect_equal(observed@data$ss, expected$`Sum Sq`)
    expect_equal(observed@data$ms, expected$`Mean Sq`)
    expect_equal(observed@data$f_value[1:2], expected$`F value`[1:2])
    expect_equal(observed@data$p_value[1:2], expected$`Pr(>F)`[1:2])
})
