#' Add or remove stat_define implementations on a test or model function
#'
#' @description
#' These are developer-interface functions intended for package authors
#' extending the `statim` framework with new model types.
#'
#' `add_stat_define()` registers a new [stat_define()] for a stat function,
#' enabling it to handle a previously unsupported variable mapper `<var_id>`.
#' Registering a model type that already exists — whether baked-in or
#' previously registered — is an error.
#'
#' `remove_stat_define()` removes a previously registered `"user"`-originated
#' entry. `"package"`-scoped entries are self-cleaning via [purge_stat_defines()]
#' called in the registering package's `.onUnload()`.
#'
#' @param stat_fn A test or model function built with [HTEST_FN()] or [MODEL_FN()]
#'   (e.g. `TTEST`, `P_TEST`).
#' @param model_type An S7 `<var_id>` class (e.g. `x_by`, `S7::class_formula`).
#' @param impl An [agendas()] object.
#' @param compatible_params A list of param S7 classes (e.g. `list(MU)`).
#'   Defaults to `list()`.
#' @param origin One of `"user"` (default) or `"package"`. Use `"user"` for
#'   interactive or script-level registration scoped to the current session.
#'   Use `"package"` inside your package's `.onLoad()`, paired with
#'   `.pkg = pkgname` — see the **Package authors** section below.
#' @param .pkg The registering package name. Required when `origin = "package"`;
#'   ignored otherwise. Pass the `pkgname` argument that R supplies to
#'   `.onLoad()` / `.onAttach()`. This is used to attribute the entry and to enable
#'   [purge_stat_defines()] to clean it up on unload.
#'
#' @section Scoping and lifecycle:
#'
#' Registrations have two scopes:
#'
#' -  *`"user"`-scoped* entries live for the duration of the R session (or
#'   until explicitly removed with [remove_stat_define()]). Use this for
#'   interactive work or in scripts.
#'
#' -  *`"package"`-scoped* entries are intended for package authors who ship
#'   extensions to `statim`. They must be registered in `.onLoad()` and
#'   cleaned up in `.onUnload()` via [purge_stat_defines()]. This keeps the
#'   registry tidy when the extending package is unloaded.
#'
#' @section Package authors:
#'
#' To ship an extension as a package, register in `zzz.R` (or any file loaded
#' early):
#'
#' ```r
#' .onLoad = function(libname, pkgname) {
#'     statim::add_stat_define(
#'         P_TEST,
#'         my_var_id,
#'         impl = agendas(
#'             base = baseline(
#'                 fn = function(.proc, .p = 0.5, .alt = "two.sided", .ci = 0.95) {
#'                     # your implementation
#'                 }
#'             )
#'         ),
#'         compatible_params = list(PI),
#'         origin = "package",
#'         .pkg = pkgname
#'     )
#' }
#'
#' .onUnload = function(libpath) {
#'     statim::purge_stat_defines("yourpackage")
#' }
#' ```
#'
#' The `.pkg = pkgname` argument is how `statim` knows which package owns
#' the entry. Without it, `origin = "package"` is an error. Never hard-code
#' the string yourself — always forward the `pkgname` R passes to `.onLoad()`.
#'
#' @return `NULL`, invisibly.
#'
#' @seealso [stat_define()], [agendas()], [remove_stat_define()],
#'   [purge_stat_defines()]
#'
#' @examples
#' # Session-scoped registration (interactive use or scripts)
#' mt = S7::new_class("my_var", parent = var_id)
#'
#' add_stat_define(
#'     P_TEST,
#'     mt,
#'     impl = agendas(
#'         base = baseline(
#'             fn = function(.proc, .value = 1) list(value = .value)
#'         )
#'     )
#' )
#'
#' # Clean up when done
#' remove_stat_define(P_TEST, mt)
#'
#' @name add-stat-define
#' @export
add_stat_define = function(
    stat_fn,
    model_type,
    impl,
    compatible_params = list(),
    origin = c("user", "package"),
    .pkg = NULL
) {
    origin = match.arg(origin)

    cls = attr(stat_fn, "cls") %||%
        cli::cli_abort(
            "{.arg stat_fn} must be a function built with {.fn HTEST_FN} or {.fn MODEL_FN}."
        )

    is_var_id_class = inherits(model_type, "S7_class") &&
        identical(model_type@parent, var_id)
    is_formula_class = identical(model_type, S7::class_formula)
    if (!is_var_id_class && !is_formula_class) {
        cli::cli_abort(
            "{.arg model_type} must be a class inheriting from {.cls var_id}, or {.code S7::class_formula}."
        )
    }

    pkg = if (identical(origin, "package")) {
        if (is.null(.pkg) || !nzchar(.pkg)) {
            cli::cli_abort(c(
                "{.code origin = \"package\"} requires a package context.",
                "i" = "Pass {.code .pkg = pkgname} where {.code pkgname} is the",
                " " = "argument received by your {.fn .onLoad} or {.fn .onAttach}.",
                "i" = "Use {.code origin = \"user\"} for session-scoped registration."
            ))
        }
        .pkg
    } else {
        NULL
    }

    new_def = stat_define(
        model_type = model_type,
        impl = impl,
        compatible_params = compatible_params
    )

    mt_name = model_type_name(model_type)
    reg_key = stat_define_registry_key(cls)
    existing_env = stat_define_registry[[reg_key]]

    # Check baked-in conflict
    baked_keys = vapply(
        get_baked_defs(stat_fn),
        function(d) model_type_name(d@model_type),
        character(1)
    )
    if (mt_name %in% baked_keys) {
        cli::cli_abort(c(
            "Model type {.val {mt_name}} is already defined as a baked-in implementation of {.fn {cls}}.",
            "i" = "Baked-in model types: {.val {baked_keys}}.",
            "i" = "Use {.fn add_variant} to extend an existing model type with a new method instead."
        ))
    }

    # Check registry conflict
    if (!is.null(existing_env) && !is.null(existing_env[[mt_name]])) {
        entry = existing_env[[mt_name]]
        conflict_origin = entry$origin
        conflict_pkg = entry$pkg %||% "unknown"
        cli::cli_abort(c(
            "Model type {.val {mt_name}} is already registered for {.fn {cls}}.",
            "i" = "Registered by: {.val {conflict_pkg}} ({conflict_origin}-scoped).",
            "i" = "Use {.fn remove_stat_define} to deregister it first, or use {.fn add_variant} to extend it."
        ))
    }

    if (is.null(existing_env)) {
        stat_define_registry[[reg_key]] = new.env(parent = emptyenv())
    }

    stat_define_registry[[reg_key]][[mt_name]] = list(
        def = new_def,
        origin = origin,
        pkg = pkg
    )
    bump_registry_version()

    invisible(NULL)
}

#' @rdname add-stat-define
#' @export
remove_stat_define = function(stat_fn, model_type) {
    cls = attr(stat_fn, "cls") %||%
        cli::cli_abort(
            "{.arg stat_fn} must be a function built with {.fn HTEST_FN} or {.fn MODEL_FN}."
        )

    is_var_id_class = inherits(model_type, "S7_class") &&
        identical(model_type@parent, var_id)
    is_formula_class = identical(model_type, S7::class_formula)
    if (!is_var_id_class && !is_formula_class) {
        cli::cli_abort(
            "{.arg model_type} must be a class inheriting from {.cls var_id}, or {.code S7::class_formula}."
        )
    }

    mt_name = model_type_name(model_type)
    reg_key = stat_define_registry_key(cls)
    existing_env = stat_define_registry[[reg_key]]

    if (is.null(existing_env) || is.null(existing_env[[mt_name]])) {
        cli::cli_abort(c(
            "Model type {.val {mt_name}} is not registered for {.fn {cls}}.",
            "i" = "Only session-registered entries can be removed manually."
        ))
    }

    entry = existing_env[[mt_name]]
    if (!identical(entry$origin, "user")) {
        cli::cli_abort(c(
            "Model type {.val {mt_name}} is {.val package}-scoped (registered by {.val {entry$pkg %||% \"unknown\"}}).",
            "i" = "Package-scoped entries are removed automatically via {.fn purge_stat_defines} on unload.",
            "i" = "Do not remove them manually."
        ))
    }

    existing_env[[mt_name]] = NULL
    bump_registry_version()
    invisible(NULL)
}

#' Purge all package-scoped stat_define registrations for a package
#'
#' Call this from your package's `.onUnload()` to clean up entries registered
#' via `add_stat_define(..., origin = "package")`.
#'
#' @param pkg A string. The package name, typically the `pkgname` argument
#'   passed to `.onUnload()`.
#'
#' @return `NULL`, invisibly.
#'
#' @examples
#' # In your package's zzz.R:
#' .onUnload = function(libpath) {
#'     statim::purge_stat_defines("yourpackage")
#' }
#'
#' @export
purge_stat_defines = function(pkg) {
    for (reg_key in ls(stat_define_registry)) {
        env = stat_define_registry[[reg_key]]
        if (!is.environment(env)) {
            next
        }
        for (mt_name in ls(env)) {
            entry = env[[mt_name]]
            if (
                identical(entry$origin, "package") && identical(entry$pkg, pkg)
            ) {
                env[[mt_name]] = NULL
            }
        }
    }
    invisible(NULL)
}

stat_define_registry = new.env(parent = emptyenv())
stat_define_registry$.version = 0L

#' @keywords internal
#' @noRd
stat_define_registry_key = function(cls) cls

detect_pkg = function(call_env) {
    top = topenv(call_env)
    pkg = environmentName(top)
    if (!nzchar(pkg) || identical(pkg, "R_GlobalEnv")) {
        return(NULL)
    }
    pkg
}

get_baked_defs = function(stat_fn) {
    # Retrieves the frozen defs list
    # From the stat function's closure
    environment(stat_fn)$defs %||% list()
}

bump_registry_version = function() {
    stat_define_registry$.version = stat_define_registry$.version + 1L
}
