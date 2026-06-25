
# statim <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/statim)](https://CRAN.R-project.org/package=statim)
[![R-CMD-check](https://github.com/s7-stats/statim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/s7-stats/statim/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/s7-stats/statim/graph/badge.svg)](https://app.codecov.io/gh/s7-stats/statim)
<!-- badges: end -->

**A Declarative Interface for Statistical Inference**

## Package Overview: Simple Fun Fact

What does `{statim}` mean?

`{statim}` is a Latin word for “immediately, at once”. The name carries
a double meaning:

- *stat*: as in statistics, the domain this package lives in

- *im* (*statim*): as in “immediate”, signalling that inference should
  be expressible as a direct declaration, not somewhat a sequence of
  mechanical steps

This simply means: you declare *what* statistical inference you want to
perform, then `{statim}` immediately delivers *how*.

## Why statim?

R has a rich statistical ecosystem. Statistical inference in general is
served by an assortment of disconnected functions: the functions you’re
looking for may exist but they are scattered across different packages.

R gained a grammar for graphics (`{ggplot2}`), and one for data
manipulation (`{dplyr}`). And then there’s `{statim}`, an attempt to
re-imagine the “grammar of statistical inference” from the ground up.
The core idea of `{statim}` in general is it’s fully declarative, and
that any inferential procedure can be described in [three
steps](#general-usage).

What makes `{statim}` *composable* for statistical workflows is the
*verbs* and the *accessibility* of the methods you’re looking for. For
example, you want to write a t-test pipeline, and you want to use the
classical one and then the permutation method. `{statim}` lets you do
that with `via("<method_name>")`, and while you can use t-test from
`default` (classical), you can access its permutation method through
`... |> via("permute")` with one line of code only. You won’t need you
to do a lot of work (which sometimes require rewriting your code), just
a single addition to the syntax.

``` r
# Classical t-test
sleep |> 
    define_model(x_by(extra, group)) |> 
    prepare_test(TTEST) |> 
    conclude()

# Permutation t-test
sleep |> 
    define_model(x_by(extra, group)) |> 
    prepare_test(TTEST) |> 
    # Here, one line added, nothing else changes
    via("permute", n = 1000L) |>         
    conclude()
```

For a quick result, the eager form skips the piped syntax entirely:

``` r
# Only works for `stat_fn` functions
TTEST(x_by(extra, group), sleep)
```

The trade-off: eager forms cannot be recalibrated / switch off into
different methods with `via()` and do not support post-execution output
manipulation ([see for more details](#core-semantics)).

## Installation

The package is yet to be submitted into CRAN.

``` r
# Stable version (not yet released)
install.packages("statim")
```

For the time being, you can install the current implementation on
GitHub:

``` r
# Development version from GitHub
# install.packages("pak")
pak::pak("s7-stats/statim")
```

## General Usage

Let us start by loading `{statim}` first:

``` r
library(statim)
```

All you need to know is that the most usual usage of `{statim}` comes
with three steps. Here’s the general anatomy of the main `{statim}`
semantics:

``` r
# Data can be piped in or passed as argument to `define_model()`
... |>                                   # Possible extensions  
    define_model(
        <var_id>(var1, var2, ...), 
        data, ...
    ) |>                                 # 1. Model definition
    # ... |>                             # Possible extensions  
    prepare(<STAT_FN>) |>                # 2. Prepare method (lazy)
    via(...) |>                          # Optional: method variant  (*)
    state_null(<expr>) |>                # Optional: null hypothesis (*)
    # ... |>                             # Possible extensions  
    conclude() |>                        # 3. Execute
    <output_handler>()                   # e.g. tidy(), display()    
```

Explanation of the code above:

1.  *Model processor and definition*, where defining the shape of model
    *to be analyzed* happens at the beginning during statistical
    inference. Typically, this step where supplying either a data frame
    or a `<var_id>` objects into `define_model()` occurs, and then some
    functions to be appended in the future updates.

2.  *Parameterization*, where the estimation process of the statistical
    inference pipeline is defined lazily. Our usual statistical
    inference application can be a model-based inference (e.g. linear
    regression through `prepare_model()`), H-test inference (e.g. t-test
    through `prepare_test()`), or both with just `prepare()`. With that
    said, the execution is lazy-loaded, and only executed if needed.
    (The `(*)` mark means they are interchangeable. )

    > `state_null()` is one of the reasons why `{statim}` — it’s
    > astronomical way of writing null hypothesis expressed
    > mathematically. Learn more about it on
    > `vignette("hypothesis-expressions", package = "statim")`.

3.  *Execution and retrieval*, where the first 2 steps is (re-)executed
    and then retrieve the output. The most common function is
    `conclude()`. There are several techniques to retrieve the output,
    e.g. through `tidy()`. This is functional if there are available
    methods are registered, automatically or from a manual step.

The `...` before the pipe above means the possible future extensions for
this package. For more information, see through `vignette("statim")`,
and learn more about how `{statim}` works.

## Core Semantics

The package is designed around three ideas:

1.  **Composability**: the simplest way to write `{statim}` has two
    forms: the eager form and the grammar/piped syntax form. The eager
    form skips the verbs and cannot be recalibrated, only skips to the
    output. On the other hand, the grammar/piped syntax form relies on
    verbs and lazy loading, which comes with the recalibration of the
    estimation method with a single `via()` call, and the execution of
    the lazy-loaded pipeline with `conclude()`.

2.  **A shared grammar**: Only applied on the main `{statim}` semantics:
    piped/grammar syntax. `define_model()` =\> `prepare()` =\>
    `conclude()` is the same shape for every inferential procedure. The
    `<var_id>` objects (`x_by`, `rel`, `pairwise`, …) describe the
    statistical structure of the problem; the verbs stay constant.

    > Eager forms (`TTEST()`, `CORTEST()`, …) provide a shortcut when
    > the full pipeline (in a form of piped syntax that reads like a
    > sentence) is not needed.

3.  **Extensible by design**: the `{statim}` pipeline is extensible. For
    instance, if you want to write new estimation method, an
    implementation is through filling up the `stat_define()` object
    (then store it within list of `defs` from `STAT_CONSTRUCTOR()`
    functions, saved as `<STAT_FN>`), then `baseline()` to write the
    default form of `<STAT_FN>` and `variant()` to extend the current
    `<STAT_FN>` form (only be accessed with `via()` only). With these,
    you can bring your own engine, your own method, your own
    implementation, or use them to extend the current ones.

## License

<!-- MIT © Joshua Marie -->

MIT + file LICENSE

## Contributing

We are sincerely grateful for contributions; they are beneficial for the
project and for us as maintainers. Please read
[CONTRIBUTING.md](CONTRIBUTING.md) for development setup, pull request
guidelines, and workflow notes.

## Code of Conduct

Please note that the statim project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
