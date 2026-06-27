# ---- tidy interface for ANOVA ----

test_that("`tidy()` returns a tibble for multi-model ANOVA (F-test)", {
    out = LifeCycleSavings |>
        write_models(f1 = sr ~ 1, f2 = sr ~ pop15, f3 = sr ~ pop15 + pop75) |>
        prepare_model(LINEAR_REG) |>
        anova() |>
        tidy()

    expect_s3_class(out, "tbl_df")
    expect_named(
        out,
        c("model", "res_df", "deviance", "df", "dev_diff", "f_value", "p_value")
    )
    expect_equal(nrow(out), 3L)
})

test_that("tidy() returns NA in first row for diff-based columns", {
    out = LifeCycleSavings |>
        write_models(f1 = sr ~ 1, f2 = sr ~ pop15) |>
        prepare_model(LINEAR_REG) |>
        anova() |>
        tidy()

    expect_true(is.na(out$df[[1]]))
    expect_true(is.na(out$dev_diff[[1]]))
    expect_true(is.na(out$f_value[[1]]))
    expect_true(is.na(out$p_value[[1]]))
    expect_true(all(is.na(out[1, c("df", "dev_diff", "f_value", "p_value")])))
})

test_that("ANOVA works on mixed `<var_id>` mappers, so `tidy.cld_anova()` must be functional", {
    expect_no_error({
        LifeCycleSavings |>
            write_models(f1 = sr ~ 1, f2 = rel(pop15, sr)) |>
            prepare_model(LINEAR_REG) |>
            anova() |>
            tidy()
    })
})

test_that("tidy() model column reflects write_models() labels", {
    out = LifeCycleSavings |>
        write_models(null = sr ~ 1, full = sr ~ pop15 + pop75) |>
        prepare_model(LINEAR_REG) |>
        anova() |>
        tidy()

    expect_equal(out$model, c("null", "full"))
})

test_that("tidy() returns a tibble for single-model (Type I) ANOVA", {
    out = LifeCycleSavings |>
        define_model(sr ~ pop15 + pop75 + dpi) |>
        prepare_model(LINEAR_REG) |>
        anova() |>
        tidy()

    expect_s3_class(out, "tbl_df")
    expect_named(out, c("term", "df", "ss", "ms", "f_value", "p_value"))
})

test_that("tidy() single-model last row is Residuals with NA test statistics", {
    out = LifeCycleSavings |>
        define_model(sr ~ pop15 + pop75) |>
        prepare_model(LINEAR_REG) |>
        anova() |>
        tidy()

    last = out[nrow(out), ]
    expect_equal(last$term, "Residuals")
    expect_true(is.na(last$f_value))
    expect_true(is.na(last$p_value))
})

test_that("tidy() single-model row count equals number of terms plus 'Residuals'", {
    out = LifeCycleSavings |>
        define_model(sr ~ pop15 + pop75 + dpi + ddpi) |>
        prepare_model(LINEAR_REG) |>
        anova() |>
        tidy()

    expect_equal(nrow(out), 5L) # It's 4 terms + 'Residuals'
})

test_that("tidy() switches stat column to chisq_value for LRT", {
    out = LifeCycleSavings |>
        write_models(f1 = sr ~ 1, f2 = sr ~ pop15) |>
        prepare_model(LINEAR_REG) |>
        anova(test = "LRT") |>
        tidy()

    expect_true("chisq_value" %in% names(out))
    expect_false("f_value" %in% names(out))
})

test_that("tidy() p-value column is numeric and in [0, 1] for non-NA rows", {
    out = LifeCycleSavings |>
        write_models(f1 = sr ~ 1, f2 = sr ~ pop15, f3 = sr ~ pop15 + pop75) |>
        prepare_model(LINEAR_REG) |>
        anova() |>
        tidy()

    p_vals = out$p_value[!is.na(out$p_value)]
    expect_true(is.numeric(p_vals))
    expect_true(all(p_vals >= 0 & p_vals <= 1))
})

test_that("tidy() residual degrees of freedom is strictly decreasing across rows", {
    out = LifeCycleSavings |>
        write_models(
            f1 = sr ~ 1,
            f2 = sr ~ pop15,
            f3 = sr ~ pop15 + pop75,
            f4 = sr ~ pop15 + pop75 + dpi
        ) |>
        prepare_model(LINEAR_REG) |>
        anova() |>
        tidy()

    expect_true(all(diff(out$res_df) < 0))
})
