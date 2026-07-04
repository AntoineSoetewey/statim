test_that("two_sample variant returns class_ttest_two", {
    on_two_sample = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "OJ"]),
            I(vc = len[supp == "VC"])
        ))) |>
        prepare_test(TTEST) |>
        via("two_sample") |>
        conclude()

    expect_s7_class(on_two_sample@data, class_ttest_two)
})

test_that("two_sample variant default weights match Welch two-sample t.test()", {
    oj = ToothGrowth$len[ToothGrowth$supp == "OJ"]
    vc = ToothGrowth$len[ToothGrowth$supp == "VC"]

    on_two_sample = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "OJ"]),
            I(vc = len[supp == "VC"])
        ))) |>
        prepare_test(TTEST) |>
        via("two_sample") |>
        conclude()

    base_two_sample = stats::t.test(oj, vc)

    expect_equal(
        on_two_sample@data@estimate,
        unname(base_two_sample$estimate[[1]] - base_two_sample$estimate[[2]]),
        tolerance = 1e-8
    )
    expect_equal(
        on_two_sample@data@t_stat,
        unname(base_two_sample$statistic),
        tolerance = 1e-8
    )
    expect_equal(
        on_two_sample@data@df,
        unname(base_two_sample$parameter),
        tolerance = 1e-8
    )
    expect_equal(
        on_two_sample@data@p_val,
        base_two_sample$p.value,
        tolerance = 1e-8
    )
})

test_that("two_sample variant .var_equal = TRUE matches pooled-variance t.test()", {
    oj = ToothGrowth$len[ToothGrowth$supp == "OJ"]
    vc = ToothGrowth$len[ToothGrowth$supp == "VC"]

    on_pooled = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "OJ"]),
            I(vc = len[supp == "VC"])
        ))) |>
        prepare_test(TTEST) |>
        via("two_sample", .var_equal = TRUE) |>
        conclude()

    base_pooled = stats::t.test(oj, vc, var.equal = TRUE)

    expect_equal(
        on_pooled@data@t_stat,
        unname(base_pooled$statistic),
        tolerance = 1e-8
    )
    expect_equal(
        on_pooled@data@df,
        unname(base_pooled$parameter),
        tolerance = 1e-8
    )
    expect_equal(on_pooled@data@p_val, base_pooled$p.value, tolerance = 1e-8)
})

test_that("two_sample variant errs when on() carries more than 2 variables", {
    expect_error(
        iris |>
            define_model(on(Sepal.Length, Sepal.Width, Petal.Length)) |>
            prepare_test(TTEST) |>
            via("two_sample") |>
            conclude(),
        "requires exactly 2 variables"
    )
})

test_that("two_sample variant with .paired = TRUE matches paired t.test()", {
    dose1 = ToothGrowth$len[ToothGrowth$supp == "OJ" & ToothGrowth$dose == 1]
    dose2 = ToothGrowth$len[ToothGrowth$supp == "VC" & ToothGrowth$dose == 1]

    on_paired = ToothGrowth |>
        with(define_model(on(
            I(d1 = len[supp == "OJ" & dose == 1]),
            I(d2 = len[supp == "VC" & dose == 1])
        ))) |>
        prepare_test(TTEST) |>
        via("two_sample", .paired = TRUE) |>
        conclude()

    base_paired = stats::t.test(dose1, dose2, paired = TRUE)

    expect_equal(
        on_paired@data@estimate,
        unname(base_paired$estimate),
        tolerance = 1e-8
    )
    expect_equal(
        on_paired@data@t_stat,
        unname(base_paired$statistic),
        tolerance = 1e-8
    )
    expect_equal(
        on_paired@data@df,
        unname(base_paired$parameter),
        tolerance = 1e-8
    )
    expect_equal(on_paired@data@p_val, base_paired$p.value, tolerance = 1e-8)
})

test_that("two_sample variant with .paired = TRUE errs on unequal-length variables", {
    expect_error(
        ToothGrowth |>
            with(define_model(on(
                I(d1 = len[supp == "OJ" & dose == 0.5]),
                I(d2 = len[supp == "VC"])
            ))) |>
            prepare_test(TTEST) |>
            via("two_sample", .paired = TRUE) |>
            conclude(),
        "same length"
    )
})

test_that("two_sample variant .w lets custom contrast weights override defaults", {
    oj = ToothGrowth$len[ToothGrowth$supp == "OJ"]
    vc = ToothGrowth$len[ToothGrowth$supp == "VC"]

    on_weighted = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "OJ"]),
            I(vc = len[supp == "VC"])
        ))) |>
        prepare_test(TTEST) |>
        via("two_sample", .w = c(oj = 1, vc = -2)) |>
        conclude()

    expect_equal(
        on_weighted@data@estimate,
        mean(oj) - 2 * mean(vc),
        tolerance = 1e-8
    )
})

test_that("state_null() on two_sample flips sign consistently when sides of == are swapped", {
    forward_claim = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "OJ"]),
            I(vc = len[supp == "VC"])
        ))) |>
        prepare_test(TTEST) |>
        via("two_sample") |>
        state_null(MU(oj) + 1 == 2 * MU(vc)) |>
        conclude()

    swapped_claim = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "OJ"]),
            I(vc = len[supp == "VC"])
        ))) |>
        prepare_test(TTEST) |>
        via("two_sample") |>
        state_null(2 * MU(vc) == MU(oj) + 1) |>
        conclude()

    expect_equal(
        swapped_claim@data@estimate,
        -forward_claim@data@estimate,
        tolerance = 1e-8
    )
    expect_equal(
        swapped_claim@data@t_stat,
        -forward_claim@data@t_stat,
        tolerance = 1e-8
    )
    expect_equal(
        swapped_claim@data@p_val,
        forward_claim@data@p_val,
        tolerance = 1e-8
    )
})

test_that("state_null() on two_sample leaves estimate unchanged when only the scalar changes", {
    small_mu_claim = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "OJ"]),
            I(vc = len[supp == "VC"])
        ))) |>
        prepare_test(TTEST) |>
        via("two_sample") |>
        state_null(MU(oj) + 1 == 2 * MU(vc)) |>
        conclude()

    large_mu_claim = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "OJ"]),
            I(vc = len[supp == "VC"])
        ))) |>
        prepare_test(TTEST) |>
        via("two_sample") |>
        state_null(MU(oj) + 5 == 2 * MU(vc)) |>
        conclude()

    expect_equal(
        small_mu_claim@data@estimate,
        large_mu_claim@data@estimate,
        tolerance = 1e-8
    )
    expect_false(isTRUE(all.equal(
        small_mu_claim@data@t_stat,
        large_mu_claim@data@t_stat
    )))
})

test_that("state_null() on two_sample handles subtraction-form intercepts identically to addition", {
    addition_claim = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "OJ"]),
            I(vc = len[supp == "VC"])
        ))) |>
        prepare_test(TTEST) |>
        via("two_sample") |>
        state_null(MU(oj) + 2 == 2 * MU(vc)) |>
        conclude()

    subtraction_claim = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "OJ"]),
            I(vc = len[supp == "VC"])
        ))) |>
        prepare_test(TTEST) |>
        via("two_sample") |>
        state_null(MU(oj) - (-2) == 2 * MU(vc)) |>
        conclude()

    expect_equal(
        addition_claim@data@t_stat,
        subtraction_claim@data@t_stat,
        tolerance = 1e-8
    )
    expect_equal(
        addition_claim@data@estimate,
        subtraction_claim@data@estimate,
        tolerance = 1e-8
    )
})

test_that("state_null() on two_sample correctly distributes a constant through parentheses", {
    oj = ToothGrowth$len[ToothGrowth$supp == "OJ"]
    vc = ToothGrowth$len[ToothGrowth$supp == "VC"]

    distributed_claim = ToothGrowth |>
        with(define_model(on(
            I(oj = len[supp == "OJ"]),
            I(vc = len[supp == "VC"])
        ))) |>
        prepare_test(TTEST) |>
        via("two_sample") |>
        state_null(2 * (MU(oj) + 1) == MU(vc)) |>
        conclude()

    expect_equal(
        distributed_claim@data@estimate,
        2 * mean(oj) - mean(vc),
        tolerance = 1e-8
    )
})

test_that("resolve_two_sample_weights defaults to a standard (1, -1) contrast when w is NULL", {
    default_weights = resolve_two_sample_weights(NULL, "oj", "vc")

    expect_equal(default_weights, c(oj = 1, vc = -1))
})

test_that("resolve_two_sample_weights reorders to (term1, term2) regardless of input order", {
    reordered_weights = resolve_two_sample_weights(
        c(vc = -2, oj = 3),
        "oj",
        "vc"
    )

    expect_equal(unname(reordered_weights), c(3, -2))
    expect_equal(names(reordered_weights), c("oj", "vc"))
})

test_that("resolve_two_sample_weights errs when w has no names at all", {
    expect_error(
        resolve_two_sample_weights(c(5, -3), "oj", "vc"),
        "must be named"
    )
})

test_that("resolve_two_sample_weights errs when one name is blank", {
    blank_named_weights = c(3, -2)
    names(blank_named_weights) = c("oj", "")

    expect_error(
        resolve_two_sample_weights(blank_named_weights, "oj", "vc"),
        "must be named"
    )
})

test_that("resolve_two_sample_weights errs when a variable from on() has no matching weight", {
    expect_error(
        resolve_two_sample_weights(c(oj = 1), "oj", "vc"),
        "omits a variable"
    )
})

test_that("resolve_two_sample_weights errs when a weight name doesn't match either variable", {
    expect_error(
        resolve_two_sample_weights(c(oj = 1, vc = -1, extra = 4), "oj", "vc"),
        "not in"
    )
})

test_that("resolve_two_sample_weights errs on a zero coefficient", {
    expect_error(
        resolve_two_sample_weights(c(oj = 0, vc = 1), "oj", "vc"),
        "non-zero"
    )
})

test_that("resolve_two_sample_weights accepts fractional coefficients unchanged", {
    fractional_weights = resolve_two_sample_weights(
        c(oj = 0.5, vc = -1.5),
        "oj",
        "vc"
    )

    expect_equal(unname(fractional_weights), c(0.5, -1.5))
})
