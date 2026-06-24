detect_pkg = function(call_env) {
    top = topenv(call_env)
    pkg = environmentName(top)
    excluded = c("", "R_GlobalEnv", "base", "testthat", "devtools", "pkgload")
    if (pkg %in% excluded || !isNamespace(top)) return(NULL)
    pkg
}

# ---- add_stat_define: happy path ----

test_that("add_stat_define registers a new model type", {
    mt = make_local_model_type()
    on.exit(remove_stat_define(P_TEST, mt), add = TRUE)

    add_stat_define(P_TEST, mt, impl = make_trivial_impl())

    reg_key = stat_define_registry_key("p_test")
    env = stat_define_registry[[reg_key]]

    expect_false(is.null(env))
    expect_false(is.null(env[[mt@name]]))
})

test_that("add_stat_define stores origin and pkg correctly for user-scoped entry", {
    mt = make_local_model_type()
    on.exit(remove_stat_define(P_TEST, mt), add = TRUE)

    add_stat_define(P_TEST, mt, impl = make_trivial_impl())

    reg_key = stat_define_registry_key("p_test")
    entry = stat_define_registry[[reg_key]][[mt@name]]

    expect_identical(entry$origin, "user")
    expect_null(entry$pkg)
})

test_that("add_stat_define stores a valid stat_define object", {
    mt = make_local_model_type()
    on.exit(remove_stat_define(P_TEST, mt), add = TRUE)

    add_stat_define(P_TEST, mt, impl = make_trivial_impl())

    reg_key = stat_define_registry_key("p_test")
    entry = stat_define_registry[[reg_key]][[mt@name]]

    expect_true(S7::S7_inherits(entry$def, stat_define))
})

# ---- add_stat_define: conflict handling ----

test_that("add_stat_define aborts on baked-in model type conflict", {
    expect_error(
        add_stat_define(P_TEST, prop, impl = make_trivial_impl()),
        regexp = "already defined as a baked-in implementation"
    )
})

test_that("add_stat_define aborts when same model type registered twice", {
    mt = make_local_model_type()
    on.exit(remove_stat_define(P_TEST, mt), add = TRUE)

    add_stat_define(P_TEST, mt, impl = make_trivial_impl())

    expect_error(
        add_stat_define(P_TEST, mt, impl = make_trivial_impl()),
        regexp = "already registered for"
    )
})

test_that("add_stat_define conflict error names the prior registrant", {
    mt = make_local_model_type()
    on.exit(remove_stat_define(P_TEST, mt), add = TRUE)

    add_stat_define(P_TEST, mt, impl = make_trivial_impl())

    err = rlang::catch_cnd(
        add_stat_define(P_TEST, mt, impl = make_trivial_impl())
    )
    expect_match(conditionMessage(err), "user-scoped")
})

test_that("add_stat_define aborts when model_type is not a var_id subclass", {
    expect_error(
        add_stat_define(P_TEST, S7::class_integer, impl = make_trivial_impl()),
        regexp = "must be a class inheriting from"
    )
})

test_that("add_stat_define aborts when stat_fn has no cls attribute", {
    mt = make_local_model_type()
    bare_fn = function(.var_id = NULL, .data = NULL, ...) {}

    expect_error(
        add_stat_define(bare_fn, mt, impl = make_trivial_impl()),
        regexp = "must be a function built with"
    )
})

# ---- add_stat_define: package origin ----

test_that("add_stat_define aborts when origin = 'package' called from global env", {
    mt = make_local_model_type()

    expect_error(
        add_stat_define(
            P_TEST,
            mt,
            impl = make_trivial_impl(),
            origin = "package"
        ),
        regexp = "requires a package context"
    )
})

# ---- remove_stat_define ----

test_that("remove_stat_define removes a user-scoped entry", {
    mt = make_local_model_type()

    add_stat_define(P_TEST, mt, impl = make_trivial_impl())
    remove_stat_define(P_TEST, mt)

    reg_key = stat_define_registry_key("p_test")
    env = stat_define_registry[[reg_key]]

    expect_true(is.null(env[[mt@name]]))
})

test_that("remove_stat_define aborts when entry does not exist", {
    mt = make_local_model_type()

    expect_error(
        remove_stat_define(P_TEST, mt),
        regexp = "not registered for"
    )
})

test_that("remove_stat_define aborts on package-scoped entry", {
    mt = make_local_model_type()
    reg_key = stat_define_registry_key("p_test")

    if (is.null(stat_define_registry[[reg_key]])) {
        stat_define_registry[[reg_key]] = new.env(parent = emptyenv())
    }
    stat_define_registry[[reg_key]][[mt@name]] = list(
        def = stat_define(model_type = mt, impl = make_trivial_impl()),
        origin = "package",
        pkg = "fauxpackage"
    )
    on.exit(
        { stat_define_registry[[reg_key]][[mt@name]] = NULL },
        add = TRUE
    )

    expect_error(
        remove_stat_define(P_TEST, mt),
        regexp = "package.*scoped"
    )
})

# ---- purge_stat_defines ----

test_that("purge_stat_defines removes only entries matching the named package", {
    mt_a = make_local_model_type()
    mt_b = make_local_model_type()

    reg_key = stat_define_registry_key("p_test")
    if (is.null(stat_define_registry[[reg_key]])) {
        stat_define_registry[[reg_key]] = new.env(parent = emptyenv())
    }

    env = stat_define_registry[[reg_key]]
    env[[mt_a@name]] = list(
        def = stat_define(model_type = mt_a, impl = make_trivial_impl()),
        origin = "package",
        pkg = "pkgA"
    )
    env[[mt_b@name]] = list(
        def = stat_define(model_type = mt_b, impl = make_trivial_impl()),
        origin = "package",
        pkg = "pkgB"
    )
    on.exit({
        env[[mt_a@name]] = NULL
        env[[mt_b@name]] = NULL
    }, add = TRUE)

    purge_stat_defines("pkgA")

    expect_null(env[[mt_a@name]])
    expect_false(is.null(env[[mt_b@name]]))
})

test_that("purge_stat_defines is a no-op when package has no registered entries", {
    expect_invisible(purge_stat_defines("nonexistent_package"))
})

# ---- build_lookup: registry-aware ----

test_that("build_lookup includes registered entries alongside baked-in", {
    mt = make_local_model_type()
    on.exit(remove_stat_define(P_TEST, mt), add = TRUE)

    add_stat_define(P_TEST, mt, impl = make_trivial_impl())

    lookup = build_lookup(environment(P_TEST)$defs, "p_test")

    expect_true("prop" %in% names(lookup))
    expect_true(mt@name %in% names(lookup))
})

test_that("build_lookup returns only baked-in entries when registry is empty", {
    mt = make_local_model_type()

    lookup = build_lookup(environment(P_TEST)$defs, "p_test")

    expect_false(mt@name %in% names(lookup))
    expect_true("prop" %in% names(lookup))
})

# ---- registry version ----

test_that("registry version increments after add_stat_define", {
    mt = make_local_model_type()
    on.exit(remove_stat_define(P_TEST, mt), add = TRUE)

    v_before = stat_define_registry$.version
    add_stat_define(P_TEST, mt, impl = make_trivial_impl())

    expect_gt(stat_define_registry$.version, v_before)
})

test_that("registry version increments after remove_stat_define", {
    mt = make_local_model_type()

    add_stat_define(P_TEST, mt, impl = make_trivial_impl())
    v_before = stat_define_registry$.version
    remove_stat_define(P_TEST, mt)

    expect_gt(stat_define_registry$.version, v_before)
})

test_that("conclude() detects stale cache when add_stat_define called after prepare_test", {
    mt = make_local_model_type()
    on.exit(remove_stat_define(P_TEST, mt), add = TRUE)

    lazy = define_model(prop(45, 100)) |> prepare_test(P_TEST)
    v_at_prepare = lazy@test_spec@registry_version

    add_stat_define(P_TEST, mt, impl = make_trivial_impl())

    expect_false(
        identical(v_at_prepare, stat_define_registry$.version)
    )
})

test_that("prepare_test stamps current registry version into test_spec", {
    mt = make_local_model_type()
    on.exit(remove_stat_define(P_TEST, mt), add = TRUE)

    add_stat_define(P_TEST, mt, impl = make_trivial_impl())
    lazy = define_model(prop(45, 100)) |> prepare_test(P_TEST)

    expect_identical(
        lazy@test_spec@registry_version,
        stat_define_registry$.version
    )
})
