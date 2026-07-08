
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

*statim* is a Latin word for “immediately, at once”. It’s prefix *stat*,
as in statistics, is where the domain this package lives in. This can be
interpreted as: you declare *what* statistical inference you want to
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
steps](https://s7-stats.github.io/statim/articles/statim.html).

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
    prepare_test(T_TEST) |> 
    conclude()

# Permutation t-test
sleep |> 
    define_model(x_by(extra, group)) |> 
    prepare_test(T_TEST) |> 
    # Here, one line added
    # Nothing else changed
    via("permute", n = 1000L) |>         
    conclude()
```

For a quick result, a one-liner or an eager form skips the piped syntax
entirely:

``` r
# Only works for `<stat_fn>` functions
T_TEST(x_by(extra, group), sleep)
```

The nuanced downside of eager forms is that they are not supported with
its main semantics that is, for example, (1) recalibrating / switching
off into different methods from the same estimation method with `via()`
and (2) do not support post-execution output manipulation.

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

    > Eager forms (`T_TEST()`, `COR_TEST()`, …) provide a shortcut when
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
