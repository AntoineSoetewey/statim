#' @title T-Test: Pairwise (`pairwise`)
#'
#' @description
#' The `pairwise` implementation performs pairwise t-tests across a set of
#' numeric variables. Each pair of variables is compared independently, and
#' results are presented as a matrix.
#'
#' Use [pairwise()] as the variable mapper `<var_id>` to select this implementation.
#'
#' @section Arguments:
#' The following arguments are passed via `...` in [T_TEST()]:
#'
#' \describe{
#'   \item{`.paired`}{Logical. Whether to perform paired comparisons.
#'     Default `FALSE`.}
#'   \item{`.mu`}{Numeric. Hypothesized mean or mean difference. Length 1
#'     (applied to all pairs) or one value per variable. Default `0`.}
#'   \item{`.alt`}{String. One of `"two.sided"`, `"greater"`, or `"less"`.
#'     Default `"two.sided"`.}
#'   \item{`.ci`}{Numeric. Confidence level. Default `0.95`.}
#' }
#'
#' @section Variants:
#' No variants are currently registered for the `pairwise` path. Use
#' [add_variant()] to register custom variants at the user or package level.
#'
#' @section Pairwise t-test class:
#' By default, it returns a [class_ttest_pairwise] object inheriting from [class_stat_infer].
#' Objects from it are printed as a pairwise matrix via [tabstats::pairwise_matrix()]. All variants that
#' also return [class_ttest_two] inherit [auto_tidy()] and [print()] automatically. Otherwise,
#' to process outputs:
#'
#' -  `print()`: Write it down through `print` from [variant()].
#' -  `tidy()`: Use [making_tidy()] to register a tidy method if needed.
#'
#' @section One-sample mode:
#' When [pairwise()] has equal referred columns, made by `direction = "<eq, lteq, gteq>"`
#' argument, each variable is tested against its own `.mu` value rather than against another
#' variable, resonating to a one-sample test. The pairwise t-test output matrix displays
#' diagonal entries only.
#'
#' @examples
#' iris |>
#'     define_model(pairwise(Sepal.Length, Sepal.Width, Petal.Length)) |>
#'     prepare_test(T_TEST) |>
#'     conclude()
#'
#' @keywords internal
#' @name ttest-pairwise
#' @family ttest-implementations
NULL

ttest_def_pairwise = test_define(
    model_type = pairwise,
    impl = agendas(
        base = baseline(fn = function(
            .proc,
            .paired = FALSE,
            .mu = 0,
            .alt = "two.sided",
            .ci = 0.95
        ) {
            var_names = .proc$var_names
            pairs = .proc$pairs
            data = .proc$data
            direction = .proc$direction %||% "lt"

            n_vars = length(var_names)

            if (length(.mu) == 1L) {
                .mu = rep(.mu, n_vars)
            } else if (length(.mu) != n_vars) {
                cli::cli_abort(c(
                    "{.arg .mu} must be length 1 or length {n_vars} (one per variable).",
                    "i" = "Variables: {.val {var_names}}.",
                    "x" = "Got length {length(.mu)}."
                ))
            }
            names(.mu) = var_names

            tests = lapply(seq_along(pairs), function(i) {
                a = pairs[[i]][[1]]
                b = pairs[[i]][[2]]
                is_one_sample = rlang::exec(identical, !!!pairs[[i]])

                res = if (is_one_sample) {
                    stats::t.test(
                        x = data[[a]],
                        mu = .mu[[a]],
                        alternative = .alt,
                        conf.level = .ci
                    )
                } else {
                    stats::t.test(
                        x = data[[a]],
                        y = data[[b]],
                        paired = .paired,
                        mu = .mu[[a]] - .mu[[b]],
                        alternative = .alt,
                        conf.level = .ci
                    )
                }

                list(a = a, b = b, ttest = res)
            })

            class_ttest_pairwise(
                var1 = vapply(tests, function(x) x[["a"]], character(1)),
                var2 = vapply(tests, function(x) x[["b"]], character(1)),
                est = vapply(
                    tests,
                    function(t) {
                        est = t$ttest$estimate
                        if (length(est) == 2L) {
                            est[[1L]] - est[[2L]]
                        } else {
                            est[[1L]]
                        }
                    },
                    numeric(1)
                ),
                df = vapply(
                    tests,
                    function(t) t$ttest$parameter[["df"]],
                    numeric(1)
                ),
                t_stat = vapply(
                    tests,
                    function(t) t$ttest$statistic[["t"]],
                    numeric(1)
                ),
                p_value = vapply(
                    tests,
                    function(t) t$ttest$p.value,
                    numeric(1)
                ),
                lower_ci = vapply(
                    tests,
                    function(t) t$ttest$conf.int[[1L]],
                    numeric(1)
                ),
                upper_ci = vapply(
                    tests,
                    function(t) t$ttest$conf.int[[2L]],
                    numeric(1)
                ),
                ci_level = .ci,
                method_name = vapply(
                    tests,
                    function(t) t$ttest$method,
                    character(1)
                )
                # method_name = unique(
                #     vapply(tests, function(t) t$ttest$method, character(1))
                # )
            )
        })
    )
)
