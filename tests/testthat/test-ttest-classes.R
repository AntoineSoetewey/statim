make_one = function(...) {
    class_ttest_one(
        term = "var",
        estimate = 1.5,
        true_mu = 0,
        df = 18,
        t_stat = 2.1,
        p_val = 0.05,
        lower_ci = 0.1,
        upper_ci = 2.9,
        ...
    )
}

make_one_multi = function(...) {
    class_ttest_one(
        term = c("a", "b", "c"),
        estimate = c(1.5, 2.0, 0.8),
        true_mu = c(0, 0, 0),
        df = c(18, 18, 18),
        t_stat = c(2.1, 3.0, 1.2),
        p_val = c(0.05, 0.01, 0.24),
        lower_ci = c(0.1, 0.5, -0.3),
        upper_ci = c(2.9, 3.5, 1.9),
        ...
    )
}

make_two = function(...) {
    class_ttest_two(
        group = "group",
        estimate = 1.5,
        t_stat = 2.1,
        df = 18,
        p_val = 0.05,
        lower_ci = 0.1,
        upper_ci = 2.9,
        ...
    )
}

make_pairwise = function(method_name = "Welch Two Sample t-test", ...) {
    class_ttest_pairwise(
        var1 = c("A", "A", "B"),
        var2 = c("B", "C", "C"),
        est = c(0.5, 1.0, 0.5),
        df = c(18, 18, 18),
        t_stat = c(1.2, 2.3, 1.1),
        p_value = c(0.24, 0.03, 0.28),
        lower_ci = c(-0.3, 0.1, -0.4),
        upper_ci = c(1.3, 1.9, 1.4),
        method_name = method_name,
        ...
    )
}

make_one_sample_pairwise = function(...) {
    vars = c("A", "B", "C")
    class_ttest_pairwise(
        var1 = vars,
        var2 = vars,
        est = c(1.0, 2.0, 3.0),
        df = c(9, 9, 9),
        t_stat = c(1.5, 2.5, 3.5),
        p_value = c(0.17, 0.03, 0.01),
        lower_ci = c(-0.5, 0.2, 1.1),
        upper_ci = c(2.5, 3.8, 4.9),
        method_name = "One Sample t-test",
        ...
    )
}

# ---- class_ttest_one: ci_level validator ----

test_that("class_ttest_one rejects ci_level of length > 1", {
    expect_error(make_one(ci_level = c(0.90, 0.95)), "length 1")
})

test_that("class_ttest_one rejects ci_level = 0", {
    expect_error(make_one(ci_level = 0), "exclusive")
})

test_that("class_ttest_one rejects ci_level = 1", {
    expect_error(make_one(ci_level = 1), "exclusive")
})

test_that("class_ttest_one rejects ci_level below 0", {
    expect_error(make_one(ci_level = -0.5), "exclusive")
})

test_that("class_ttest_one rejects ci_level above 1", {
    expect_error(make_one(ci_level = 1.5), "exclusive")
})

test_that("class_ttest_one accepts valid ci_level", {
    expect_no_error(make_one(ci_level = 0.90))
    expect_no_error(make_one(ci_level = 0.95))
    expect_no_error(make_one(ci_level = 0.99))
})

# ---- class_ttest_one: print ----

test_that("print() on class_ttest_one returns invisibly", {
    expect_invisible(print(make_one()))
})

test_that("print() on class_ttest_one produces output", {
    expect_output(print(make_one()))
})

test_that("print() on class_ttest_one (multi) returns invisibly", {
    expect_invisible(print(make_one_multi()))
})

test_that("print() on class_ttest_one (multi) produces output", {
    expect_output(print(make_one_multi()))
})

# ---- class_ttest_two: ci_level validator ----

test_that("class_ttest_two rejects ci_level of length > 1", {
    expect_error(make_two(ci_level = c(0.90, 0.95)), "length 1")
})

test_that("class_ttest_two rejects ci_level = 0", {
    expect_error(make_two(ci_level = 0), "exclusive")
})

test_that("class_ttest_two rejects ci_level = 1", {
    expect_error(make_two(ci_level = 1), "exclusive")
})

test_that("class_ttest_two rejects ci_level below 0", {
    expect_error(make_two(ci_level = -0.5), "exclusive")
})

test_that("class_ttest_two rejects ci_level above 1", {
    expect_error(make_two(ci_level = 1.5), "exclusive")
})

test_that("class_ttest_two accepts valid ci_level", {
    expect_no_error(make_two(ci_level = 0.90))
    expect_no_error(make_two(ci_level = 0.95))
    expect_no_error(make_two(ci_level = 0.99))
})

# ---- class_ttest_pairwise: ci_level validator ----

test_that("class_ttest_pairwise rejects ci_level of length > 1", {
    expect_error(make_pairwise(ci_level = c(0.90, 0.95)), "length 1")
})

test_that("class_ttest_pairwise rejects ci_level = 0", {
    expect_error(make_pairwise(ci_level = 0), "exclusive")
})

test_that("class_ttest_pairwise rejects ci_level = 1", {
    expect_error(make_pairwise(ci_level = 1), "exclusive")
})

test_that("class_ttest_pairwise rejects ci_level below 0", {
    expect_error(make_pairwise(ci_level = -0.1), "exclusive")
})

test_that("class_ttest_pairwise rejects ci_level above 1", {
    expect_error(make_pairwise(ci_level = 1.01), "exclusive")
})

test_that("class_ttest_pairwise accepts valid ci_level", {
    expect_no_error(make_pairwise(ci_level = 0.90))
    expect_no_error(make_pairwise(ci_level = 0.95))
})

# ---- class_ttest_pairwise: print ----

test_that("print() on class_ttest_pairwise (two-sample) returns invisibly", {
    expect_invisible(print(make_pairwise()))
})

test_that("print() on class_ttest_pairwise (two-sample) produces output", {
    expect_output(print(make_pairwise()))
})

test_that("print() on class_ttest_pairwise (two-sample) detects off-diagonal pairs", {
    x = make_pairwise()
    expect_false(all(x@var1 == x@var2))
})

test_that("print() on class_ttest_pairwise (one-sample) returns invisibly", {
    expect_invisible(print(make_one_sample_pairwise()))
})

test_that("print() on class_ttest_pairwise (one-sample) produces output", {
    expect_output(print(make_one_sample_pairwise()))
})

test_that("print() on class_ttest_pairwise (one-sample) detects diagonal pairs", {
    x = make_one_sample_pairwise()
    expect_true(all(x@var1 == x@var2))
})

test_that("print() uses method_name as title when length-1", {
    expect_output(print(make_pairwise()), "Welch Two Sample t-test")
})

test_that("print() falls back to 'Pairwise t-Tests' when method_name has length > 1", {
    x = make_pairwise(method_name = c("Welch Two Sample t-test", "One Sample t-test"))
    expect_output(print(x), "Pairwise t-Tests")
})
