test_that("build_lookup() keys defs by model_type name", {
    lookup = build_lookup(list(ttest_def_two), cls = "ttest")

    expect_named(lookup, "x_by")
    expect_s7_class(lookup[["x_by"]], stat_define)
})

test_that("build_lookup() last-write-wins on duplicate model_type", {
    lookup = build_lookup(list(ttest_def_two, ttest_def_two), cls = "ttest")

    expect_length(lookup, 1)
})

test_that("find_def() returns the matched def", {
    lookup = build_lookup(list(ttest_def_two), cls = "ttest")
    def = find_def(lookup, "x_by")

    expect_s7_class(def, stat_define)
})

test_that("find_def() errors on unknown model type", {
    lookup = build_lookup(list(ttest_def_two), cls = "ttest")

    expect_error(find_def(lookup, "nonexistent"), class = "rlang_error")
})

test_that("model_type_name() extracts name from S7_class", {
    expect_equal(model_type_name(x_by), "x_by")
    expect_equal(model_type_name(rel), "rel")
    expect_equal(model_type_name(pairwise), "pairwise")
})

test_that("get_model_type() extracts name from an instantiated S7 model ID", {
    expect_equal(get_model_type(x_by(extra, group)), "x_by")
    expect_equal(get_model_type(rel(speed, dist)), "rel")
    expect_equal(get_model_type(extra ~ group), "formula")
})

test_that("impl_cls_from_model() concatenates stat_cls and model name", {
    expect_equal(impl_cls_from_model("ttest", x_by(extra, group)), "ttest_x_by")
    expect_equal(
        impl_cls_from_model("linear_reg", rel(speed, dist)),
        "linear_reg_rel"
    )
    expect_equal(impl_cls_from_model("ttest", extra ~ group), "ttest_formula")
})

# ---- testing inject_and_run ----

test_that("inject_and_run() passes processed as .proc and runs fn", {
    impl = ttest_def_two@impl$base
    processed = model_processor(x_by(extra, group), sleep)

    result = inject_and_run(impl, processed, args = list())

    expect_s7_class(result, class_ttest_two)
    expect_true(length(result@group) > 0L)
})

test_that("inject_and_run() prefers user args over fn defaults", {
    impl = ttest_def_two@impl$base
    processed = model_processor(x_by(extra, group), sleep)

    expect_no_error(inject_and_run(
        impl,
        processed,
        args = list(.paired = FALSE)
    ))
})

test_that("inject_and_run() errors when required arg is missing", {
    impl = baseline(fn = function(.proc, required_arg) required_arg)
    processed = model_processor(x_by(extra, group), sleep)

    expect_error(
        inject_and_run(impl, processed, args = list()),
        class = "rlang_error"
    )
})

test_that("baseline() errors when fn does not start with .proc", {
    expect_error(
        baseline(fn = function(x, group_data) x),
        class = "rlang_error"
    )
})

test_that("variant() errors when fn does not start with .proc", {
    expect_error(variant(fn = function(x, group_data) x), class = "rlang_error")
})

test_that("inject_and_run() errors on unknown arg when fn has no ...", {
    impl = ttest_def_two@impl$base
    processed = model_processor(x_by(extra, group), sleep)

    expect_error(
        inject_and_run(impl, processed, args = list(foo = 1)),
        class = "rlang_error"
    )
})

test_that("inject_and_run() errors on misspelled known arg", {
    impl = ttest_def_two@impl$base
    processed = model_processor(x_by(extra, group), sleep)

    expect_error(
        inject_and_run(impl, processed, args = list(.cii = 0.90)),
        class = "rlang_error"
    )
})

test_that("inject_and_run() error message lists accepted args", {
    impl = ttest_def_two@impl$base
    processed = model_processor(x_by(extra, group), sleep)

    err = rlang::catch_cnd(inject_and_run(
        impl,
        processed,
        args = list(foo = 1)
    ))
    full_msg = rlang::cnd_message(err)

    expect_match(full_msg, "foo")
    expect_match(full_msg, "\\.paired", fixed = FALSE)
})

test_that("inject_and_run() with extra args succeeds when fn has ...", {
    impl = baseline(fn = function(.proc, ...) list(...))
    processed = model_processor(x_by(extra, group), sleep)

    result = inject_and_run(impl, processed, args = list(foo = 1, bar = 2))

    expect_equal(result, list(foo = 1, bar = 2))
})

test_that("inject_and_run() does not error when args is empty", {
    impl = ttest_def_two@impl$base
    processed = model_processor(x_by(extra, group), sleep)

    expect_no_error(inject_and_run(impl, processed, args = list()))
})

# stat_infer_spec / cld_exec ---------------------------------------------------

test_that("stat_infer_spec() stores data and metadata", {
    spec = stat_infer_spec(
        data = list(x = 1),
        impl_cls = "ttest_x_by",
        stat_cls = "ttest",
        print_fn = NULL,
        name = "T-Test"
    )

    expect_s7_class(spec, stat_infer_spec)
    expect_equal(spec@name, "T-Test")
    expect_equal(spec@stat_cls, "ttest")
})

test_that("cld_exec inherits from stat_infer_spec", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        conclude()

    expect_s7_class(result, cld_exec)
    expect_s7_class(result, stat_infer_spec)
})

test_that("cld_exec@cld_meta contains required fields", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        conclude()

    expect_named(
        result@cld_meta,
        c("var_id", "processed", "stat_name", "method", "data_name"),
        ignore.order = TRUE
    )
    expect_equal(result@cld_meta$method, "default")
    expect_equal(result@cld_meta$stat_name, "T-Test")
})

test_that("print.cld_exec() returns invisibly", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(T_TEST) |>
        conclude()

    expect_invisible(print(result))
})

# ---- test_spec / model_spec ----

test_that("HTEST_FN() with NULL .model returns a test_spec", {
    spec = T_TEST(.model = NULL)

    expect_s7_class(spec, test_spec)
    expect_equal(spec@cls, "ttest")
    expect_equal(spec@name, "T-Test")
})

test_that("MODEL_FN() with NULL .model returns a model_spec", {
    spec = LINEAR_REG(.model = NULL)

    expect_s7_class(spec, model_spec)
    expect_equal(spec@cls, "linear_reg")
})

test_that("test_spec and model_spec are not interchangeable", {
    expect_false(S7::S7_inherits(T_TEST(.model = NULL), model_spec))
    expect_false(S7::S7_inherits(LINEAR_REG(.model = NULL), test_spec))
})
