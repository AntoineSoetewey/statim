dummy_impl = agendas(
    base = baseline(
        fn = function(x, .ci = 0.95) list(mean = mean(x), ci = .ci),
        print = function(x, ...) invisible(x)
    ),
    boot = variant(
        fn = function(x, n = 1000L) list(mean = mean(x), n = n)
    )
)

test_that("test_define creates correct S7 object", {
    td = test_define(
        model_type = "x_by",
        impl_class = "mytest_two",
        impl = dummy_impl
    )
    expect_true(S7::S7_inherits(td, test_define))
    expect_equal(td@model_type, "x_by")
    expect_equal(td@impl_class, "mytest_two")
    expect_s3_class(td@impl, "agendas")
})

test_that("test_define errors on wrong impl type", {
    expect_error(
        test_define(model_type = "x_by", impl_class = "t", impl = list()),
        regexp = "agendas"
    )
})

test_that("test_define errors on non-string model_type", {
    expect_error(
        test_define(model_type = 1L, impl_class = "t", impl = dummy_impl)
    )
})

test_that("test_define errors on non-string impl_class", {
    expect_error(
        test_define(model_type = "x_by", impl_class = 1L, impl = dummy_impl)
    )
})

test_that("test_define stores compatible_params", {
    td = test_define(
        model_type = "x_by",
        impl_class = "mytest_two",
        impl = dummy_impl,
        compatible_params = c("MU")
    )
    expect_equal(td@compatible_params, "MU")
})

test_that("agendas base is a baseline object", {
    expect_true(S7::S7_inherits(dummy_impl$base, baseline))
})

test_that("agendas variants are variant objects", {
    expect_true(S7::S7_inherits(dummy_impl$variants$boot, variant))
})

test_that("baseline stores fn correctly", {
    fn = function(x, .ci = 0.95) x
    b = baseline(fn = fn)
    expect_identical(b@fn, fn)
})

test_that("variant stores fn correctly", {
    fn = function(x, n = 100L) x
    v = variant(fn = fn)
    expect_identical(v@fn, fn)
})
