PLAIN_TEST = HTEST_FN(
    cls = "plain_test",
    defs = list(test_define(
        model_type = prop,
        impl = agendas(
            base = baseline(fn = function(.proc, .p = 0.5) {
                list(x = .proc$x, n = .proc$n, p = .p)
            }),
            with_p = variant(fn = function(.proc, .p = 0.5) {
                list(x = .proc$x, n = .proc$n, p = .p)
            })
        ),
        compatible_params = list(PI)
    )),
    .name = "Plain Test"
)

plain_pipeline = function(variant_name = NULL) {
    p = define_model(prop(45, 100)) |> prepare_test(PLAIN_TEST)
    if (!is.null(variant_name)) {
        p = via(p, variant_name)
    }
    conclude(p)
}

register_plain_tidy = function() {
    making_tidy(PLAIN_TEST, prop) %<-%
        method_tidy(default = function(.x, ...) {
            d = .x@data
            tibble::tibble(x = d$x, n = d$n, p = d$p)
        })
}

# ---- auto_tidy() guard ----

test_that("auto_tidy() errors on non-class_stat_infer input", {
    expect_error(
        auto_tidy(list(x = 1)),
        regexp = "`x` must inherit",
        class = "rlang_error"
    )
    expect_error(
        auto_tidy(data.frame(x = 1)),
        regexp = "Got <data.frame>",
        class = "rlang_error"
    )
})

test_that("auto_tidy() errors with no registered method on bare class_stat_infer subclass", {
    bare = S7::new_class("bare_infer", parent = class_stat_infer)
    expect_error(
        auto_tidy(bare()),
        regexp = "No.*auto_tidy.*method",
        class = "rlang_error"
    )
})

# ---- tidy() -> auto_tidy() dispatch --------

test_that("tidy() dispatches auto_tidy() for class_stat_infer and returns correct shape", {
    out = define_model(prop(45, 100)) |>
        prepare_test(P_TEST) |>
        conclude() |>
        tidy()

    expect_s3_class(out, "tbl_df")
    expect_equal(nrow(out), 1L)
    expect_named(
        out,
        c(
            "successes",
            "total",
            "true_p",
            "estimate",
            "statistic",
            "p_val",
            "lower_95",
            "upper_95"
        )
    )
})

# ---- no making_tidy entry ----

test_that("tidy() errors when no registry entry exists for a plain-return stat", {
    exec = plain_pipeline()
    expect_error(
        tidy(exec),
        regexp = "No tidy method found",
        class = "rlang_error"
    )
})

# ---- variant name absent in registry ----

test_that("tidy() errors when the variant name has no making_tidy entry", {
    withr::with_environment(new.env(parent = emptyenv()), {
        register_plain_tidy()
        exec = plain_pipeline(variant_name = "with_p")
        expect_error(
            tidy(exec),
            regexp = "No tidy entry for variant",
            class = "rlang_error"
        )
    })
})

# ---- method_tidy() constructor ----

test_that("method_tidy() errors when default is not a function", {
    expect_error(
        method_tidy(default = "not_a_function"),
        regexp = "`default` must be a function",
        class = "rlang_error"
    )
})

test_that("method_tidy() errors when a variant entry is not a function", {
    good_fn = function(.x, ...) tibble::tibble()
    expect_error(
        method_tidy(default = good_fn, bad_variant = "not_a_function"),
        regexp = "All variant entries must be functions",
        class = "rlang_error"
    )
})

# ---- making_tidy_register() validation ----

test_that("making_tidy() errors when obj has no cls attribute", {
    raw_fn = function() NULL
    expect_error(
        making_tidy(raw_fn, prop) %<-%
            method_tidy(default = function(.x, ...) tibble::tibble()),
        regexp = "`obj` must be a function built with",
        class = "rlang_error"
    )
})

test_that("making_tidy() errors when model_type is not a var_id subclass or class_formula", {
    expect_error(
        making_tidy(P_TEST, list()) %<-%
            method_tidy(default = function(.x, ...) tibble::tibble()),
        regexp = "must be a class inheriting from",
        class = "rlang_error"
    )
})

test_that("making_tidy() errors when RHS is not a method_tidy object", {
    expect_error(
        making_tidy(P_TEST, prop) %<-% list(default = function(.x, ...) NULL),
        regexp = "Right-hand side of.*must be a.*method_tidy",
        class = "rlang_error"
    )
})
