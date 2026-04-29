dummy_baseline = baseline(
    fn = function(x, .ci = 0.95) list(mean = mean(x), ci = .ci),
    print = function(x, ...) invisible(x)
)

dummy_variant = variant(
    fn = function(x, n = 1000L) list(mean = mean(x), n = n),
    print = function(x, ...) invisible(x)
)

dummy_agendas = agendas(
    base = dummy_baseline,
    boot = dummy_variant
)

dummy_def = test_define(
    model_type = "x_by",
    impl_class = "dummy_two",
    impl = dummy_agendas
)

dummy_fn = HTEST_FN(
    cls = "dummy",
    defs = list(dummy_def),
    .name = "Dummy Test"
)

# ---- baseline() ----

test_that("baseline() requires default to be a function", {
    expect_error(baseline(fn = 42))
})

test_that("baseline() requires print to be a function or NULL", {
    expect_error(baseline(fn = function(x) x, print = 42))
})

test_that("baseline() constructs correctly", {
    b = baseline(fn = function(x) x)
    expect_true(S7::S7_inherits(b, baseline))
    expect_type(b@fn, "closure")
    expect_null(b@print)
})

# ---- variant() ----

test_that("variant() requires default to be a function", {
    expect_error(variant(fn = 42))
})

test_that("variant() constructs correctly", {
    v = variant(fn = function(x, n = 100L) x)
    expect_true(S7::S7_inherits(v, variant))
    expect_type(v@fn, "closure")
    expect_null(v@print)
})

# ---- agendas() ----

test_that("agendas() requires base argument", {
    expect_error(agendas())
})

test_that("agendas() requires base to be a baseline object", {
    expect_error(agendas(base = list()))
})

test_that("agendas() errors on unnamed variants", {
    expect_error(agendas(base = dummy_baseline, dummy_variant, named = dummy_variant))
})

test_that("agendas() errors if variant is not a variant object", {
    expect_error(agendas(base = dummy_baseline, boot = list()))
})

test_that("agendas() constructs correctly", {
    a = agendas(base = dummy_baseline, boot = dummy_variant)
    expect_s3_class(a, "agendas")
    expect_true(S7::S7_inherits(a$base, baseline))
    expect_true(S7::S7_inherits(a$variants$boot, variant))
})

# ---- test_define() ----

test_that("test_define() requires model_type as single string", {
    expect_error(test_define(model_type = 1L, impl_class = "t", impl = dummy_agendas))
})

test_that("test_define() requires impl to be an agendas object", {
    expect_error(test_define(model_type = "x_by", impl_class = "t", impl = list()))
})

test_that("test_define() constructs correctly", {
    expect_true(S7::S7_inherits(dummy_def, test_define))
    expect_equal(dummy_def@model_type, "x_by")
    expect_equal(dummy_def@impl_class, "dummy_two")
})

# ---- build_lookup() / find_def() ----

test_that("build_lookup() keys by model_type", {
    lookup = build_lookup(list(dummy_def))
    expect_named(lookup, "x_by")
})

test_that("build_lookup() last-writer-wins on duplicate model_type", {
    def2 = test_define(
        model_type = "x_by",
        impl_class = "dummy_three",
        impl = dummy_agendas
    )
    lookup = build_lookup(list(dummy_def, def2))
    expect_equal(lookup[["x_by"]]@impl_class, "dummy_three")
})

test_that("find_def() returns correct definition", {
    lookup = build_lookup(list(dummy_def))
    result = find_def(lookup, model_type = "x_by")
    expect_equal(result@impl_class, "dummy_two")
})

test_that("find_def() errors when model_type not found", {
    lookup = build_lookup(list(dummy_def))
    expect_error(find_def(lookup, model_type = "rel"))
})

# ---- new_htest() ----

test_that("new_htest() creates htest_spec with correct classes", {
    result = new_htest(list(x = 1), impl_cls = "dummy_two", test_cls = "dummy")
    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "dummy_two")
    expect_s3_class(result, "dummy")
    expect_equal(result$data, list(x = 1))
})

test_that("new_htest() stores print_fn", {
    printer = function(x, ...) invisible(x)
    result = new_htest(list(), impl_cls = "dummy_two", test_cls = "dummy", print_fn = printer)
    expect_identical(attr(result, "print_fn"), printer)
})

test_that("print.htest_spec calls print_fn when set", {
    called = FALSE
    printer = function(x, ...) { called <<- TRUE; invisible(x) }
    obj = new_htest(list(val = 42), "dummy_two", "dummy", print_fn = printer)
    print(obj)
    expect_true(called)
})

test_that("print.htest_spec falls back to print(x$data) when no print_fn", {
    obj = new_htest(42, "dummy_two", "dummy")
    expect_invisible(print(obj))
})

# ---- HTEST_FN() ----

test_that("HTEST_FN() returns a function with cls attribute", {
    expect_type(dummy_fn, "closure")
    expect_equal(attr(dummy_fn, "cls"), "dummy")
})

test_that("HTEST_FN() returns test_spec when called with no model", {
    spec = dummy_fn()
    expect_s3_class(spec, "test_spec")
    expect_equal(spec$cls, "dummy")
    expect_equal(spec$name, "Dummy Test")
})
