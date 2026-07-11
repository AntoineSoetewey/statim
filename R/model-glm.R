#' Generalized linear model
#'
#' A modified GLM for `{statim}` pipeline passed through [stats::glm()].
#'
#' Additional arguments are passed to [stats::glm()]. The most important
#' is `family`, which controls the error distribution and link function
#' (e.g. [stats::binomial()], [stats::poisson()]). Defaults to
#' [stats::gaussian()] when omitted.
#'
#' @param .var_id A variable mapper `<var_id>` from [define_model()], or `NULL` to return a
#'   `model_spec` for use in [prepare_model()].
#' @param .data A data frame. Used when `.var_id` is supplied directly.
#' @param ... Additional arguments passed to [stats::glm()].
#'
#' @return A `cld_exec` object in a `class_glm_object`, or a `model_spec`
#'   when `.var_id = NULL`.
#'
#' @examples
#' # logistic regression
#' mtcars |>
#'     define_model(am ~ wt + hp) |>
#'     prepare_model(GLM) |>
#'     update(family = binomial()) |>
#'     conclude()
#'
#' \dontrun{
#' # model comparison via anova()
#' mod1 = mtcars |>
#'     define_model(am ~ 1) |>
#'     prepare_model(GLM) |>
#'     update(family = binomial()) |>
#'     conclude()
#' mod2 = mtcars |>
#'     define_model(am ~ wt) |>
#'     prepare_model(GLM) |>
#'     update(family = binomial()) |>
#'     conclude()
#' mod3 = mtcars |>
#'     define_model(am ~ wt + hp) |>
#'     prepare_model(GLM) |>
#'     update(family = binomial()) |>
#'     conclude()
#'
#' anova(mod1, mod2, mod3)
#' }
#'
#' @export
GLM = MODEL_FN(
    cls = "glm",
    defs = list(glm_def_rel, glm_def_formula),
    .name = "Generalized Linear Model"
)

#' Structured result container for GLM fits
#'
#' @description
#' An S7 class produced by [GLM] pipelines. Not constructed manually —
#' use `define_model() |> prepare_model(GLM) |> conclude()` instead.
#'
#' Inherits from [anova_able], so it participates in [anova()] directly.
#' Downstream packages can use it as a `parent` in `S7::new_class()`.
#'
#' @usage NULL
#'
#' @details
#' Constructor arguments (populated automatically by [GLM]):
#'
#' - `terms`: model terms object.
#' - `df_residual`: residual degrees of freedom.
#' - `deviance`: scalar deviance.
#' - `dispersion`: scalar dispersion parameter.
#' - `family`: string naming the error family, e.g. `"binomial"`.
#' - `link`: string naming the link function, e.g. `"logit"`.
#' - `null_deviance`: scalar deviance of the intercept-only model.
#' - `aic`: scalar AIC.
#' - `logLik`: scalar log-likelihood of the fitted model.
#' - `null_logLik`: scalar log-likelihood of the intercept-only model.
#' - `beta`: named numeric vector of coefficient estimates.
#' - `std_beta`: named numeric vector of coefficient standard errors.
#' - `actual`: numeric vector of the original values on the response scale.
#' - `fitted`: numeric vector of fitted values on the response scale.
#' - `vcov`: variance-covariance matrix of the coefficients, e.g.
#'   `stats::vcov(fit)`. Required for [predict()] with `interval`.
#' - `x_mat`: model matrix stored as a flat numeric vector via
#'   `as.numeric(stats::model.matrix(fit))`. Required for [predict()].
#' - `x_levels`: factor levels used when fitting, via
#'   `stats::.getXlevels(fit$terms, stats::model.frame(fit))`. Required
#'   for [predict()] on new data with factor predictors.
#'
#' The following are computed automatically and do not need to be supplied:
#'
#' - `statistic`: per-coefficient test statistics (`beta / std_beta`).
#' - `p_value`: per-coefficient two-sided p-values. Uses a z-test when
#'   `family` is `"binomial"` or `"poisson"` (fixed dispersion), and a
#'   t-test against `df_residual` otherwise (estimated dispersion).
#' - `coefficients`: tibble with columns `term`, `estimate`, `std_error`,
#'   `statistic`, `p_value`.
#' - `fit_summary`: tibble with columns `family`, `link`, `null_deviance`,
#'   `deviance`, `df_residual`, `aic`, `n_obs`.
#'
#' @section predict() arguments:
#' [predict()] on a `class_glm_object` accepts:
#'
#' - `new_data`: A data frame of new predictors. `NULL` (the default)
#'   returns fitted values and response-based `truth` for the training data.
#' - `type`: One of `"response"` (default, back-transformed through the
#'   inverse link) or `"link"` (linear predictor scale).
#' - `interval`: One of `"none"` (default) or `"confidence"`.
#'   Prediction intervals are not available, since GLMs have no closed-form
#'   analogue of OLS prediction error.
#' - `level`: Confidence level for the interval. Default `0.95`.
#'
#' @seealso [anova_able], [GLM]
#'
#' @examples
#' # Inheriting from class_glm_object in a downstream package:
#' my_glm = S7::new_class(
#'     "my_glm",
#'     parent = class_glm_object
#' )
#'
#' # Populating class_glm_object from a fitted glm (as done internally):
#' fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
#' s = summary(fit)
#' fam = fit$family$family
#'
#' obj = class_glm_object(
#'     terms = fit$terms,
#'     df_residual = fit$df.residual,
#'     deviance = fit$deviance,
#'     dispersion = if (fam %in% c("binomial", "poisson")) 1 else s$dispersion,
#'     family = fam,
#'     link = fit$family$link,
#'     null_deviance = fit$null.deviance,
#'     aic = fit$aic,
#'     beta = coef(s)[, 1],
#'     std_beta = coef(s)[, 2],
#'     actual = unname(fit$y),
#'     fitted = unname(fit$fitted.values),
#'     vcov = vcov(fit),
#'     x_mat = as.numeric(model.matrix(fit)),
#'     x_levels = .getXlevels(fit$terms, model.frame(fit))
#' )
#'
#' obj@coefficients
#' obj@fit_summary
#'
#' @export
class_glm_object = S7::new_class(
    "glm_object",
    parent = anova_able,
    properties = list(
        # ---- Required inputs ----
        beta = S7::class_numeric,
        std_beta = S7::class_numeric,
        link = S7::new_property(
            class = S7::class_character,
            default = "identity"
        ),
        null_deviance = S7::class_numeric,
        aic = S7::class_numeric,
        logLik = S7::class_numeric,
        null_logLik = S7::class_numeric,

        # ---- For prediction purposes ----
        actual = S7::class_numeric,
        fitted = S7::class_numeric,
        vcov = S7::class_numeric,
        x_mat = S7::class_numeric,
        x_assign = S7::new_property(
            class = S7::new_union(S7::class_integer, S7::class_missing),
            default = NULL
        ),
        x_levels = S7::new_property(
            class = S7::new_union(S7::class_list, S7::class_missing),
            default = NULL
        ),

        # ---- Computed: per-coefficient stats ----
        statistic = S7::new_property(getter = function(self) {
            self@beta / self@std_beta
        }),
        p_value = S7::new_property(getter = function(self) {
            if (self@family %in% c("binomial", "poisson")) {
                2 * pnorm(abs(self@statistic), lower.tail = FALSE)
            } else {
                2 *
                    pt(
                        abs(self@statistic),
                        df = self@df_residual,
                        lower.tail = FALSE
                    )
            }
        }),

        # ---- Computed: coefficients table ----
        coefficients = S7::new_property(getter = function(self) {
            tibble::tibble(
                term = names(self@beta),
                estimate = unname(self@beta),
                std_error = unname(self@std_beta),
                statistic = unname(self@statistic),
                p_value = unname(self@p_value)
            )
        }),

        # ---- Computed: model fit summary ----
        fit_summary = S7::new_property(getter = function(self) {
            tibble::tibble(
                family = self@family,
                link = self@link,
                null_deviance = self@null_deviance,
                deviance = self@deviance,
                df_residual = as.integer(self@df_residual),
                aic = self@aic,
                n_obs = as.integer(self@df_residual + length(self@beta))
            )
        })
    )
)

S7::method(print, class_glm_object) = function(x, ...) {
    pval_styler = function(x) {
        x_num = suppressWarnings(as.numeric(x$value))
        if (is.na(x_num) || x_num > 0.05) {
            cli::style_italic(x$value)
        } else if (x_num > 0.01) {
            cli::col_red(x$value)
        } else {
            cli::style_bold("<0.001")
        }
    }

    cli::cat_line(cli::rule(left = "Coefficients", line = "-"), "\n")
    tabstats::table_default(
        x@coefficients,
        style_columns = tabstats::td_style(p_value = pval_styler),
        nrows = nrow(x@coefficients),
        justify_cols = list(term = "left"),
        vb = list(char = "\u2502", after = "term")
    )
    cat("\n\n")

    cli::cat_line(cli::rule(left = "Model Fit", line = "-"), "\n")
    tabstats::table_default(x@fit_summary)
    cat("\n\n")

    invisible(x)
}

#' Extract slots from a fitted glm into a class_glm_object
#'
#' @param fit A fitted `glm` object.
#' @return A `class_glm_object`.
#'
#' @keywords internal
#' @noRd
glm_to_glm_object = function(fit) {
    if (!inherits(fit, "glm")) {
        cli::cli_abort(c(
            "{.fn glm_to_glm_object} requires a fitted {.cls glm} object.",
            "i" = "Got {.cls {class(fit)[[1]]}}."
        ))
    }

    s = summary(fit)
    coef_mat = s$coefficients

    fam = fit$family$family
    phi = if (fam %in% c("binomial", "poisson")) {
        1
    } else {
        s$dispersion
    }

    null_formula = stats::update(stats::formula(fit), . ~ 1)
    null_fit = stats::glm(null_formula, data = fit$model, family = fit$family)

    mm = stats::model.matrix(fit)
    xlev = stats::.getXlevels(fit$terms, stats::model.frame(fit)) %||% list()

    class_glm_object(
        terms = fit$terms,
        df_residual = fit$df.residual,
        deviance = fit$deviance,
        dispersion = phi,
        family = fam,
        link = fit$family$link,
        null_deviance = fit$null.deviance,
        aic = fit$aic,
        logLik = as.numeric(stats::logLik(fit)),
        null_logLik = as.numeric(stats::logLik(null_fit)),
        beta = coef_mat[, 1],
        std_beta = coef_mat[, 2],
        actual = unname(fit$y),
        fitted = unname(fit$fitted.values),
        vcov = stats::vcov(fit),
        x_mat = as.numeric(mm),
        x_assign = attr(mm, "assign"),
        x_levels = xlev
    )
}

S7::method(n_params, class_glm_object) = function(model) {
    length(model@beta)
}
