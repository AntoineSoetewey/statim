ttest_def_permute_rfast = test_define(
    model_type = "x_by",
    impl_class = "ttest_permute_rfast",
    engine = "rfast",
    method = method_spec(
        "permute",
        method_type = "replicate",
        defaults = list(B = 999L)
    ),
    vars = list(
        x = function(p) p$x_data[[1]],
        group = function(p) p$group_data[[1]]
    ),
    run = function(self) {
        rlang::check_installed(
            "Rfast2",
            reason = "to run the Rfast2-backed permutation t-test engine"
        )

        B = ic_method_arg(self, "B")
        grp = as.character(ic_pull(self, "group"))
        resp = ic_pull(self, "x")
        lvls = unique(grp)

        if (length(lvls) != 2L) {
            cli::cli_abort(c(
                "Permutation t-test requires exactly 2 groups.",
                "i" = "Found {length(lvls)} group{{?s}}."
            ))
        }

        x = resp[grp == lvls[[1]]]
        y = resp[grp == lvls[[2]]]

        # Rfast2 requires numeric vectors, no NAs
        res = Rfast2::perm.ttest(x = x, y = y, B = B)

        list(
            stat = res[["stat"]],
            p.value = res[["permutation p-value"]],
            B = B
        )
    },
    print = function(x, ...) {
        summary_data = tibble::tibble(
            Statistic = round(x$data$stat, 4),
            `p-value` = round(x$data$p.value, 4),
            n_perms = x$data$B
        )

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

        cli::cat_line(cli::rule(center = "T-test Permutation", line = "="), "\n\n")
        cli::cat_line(cli::rule(left = "Summary", line = "-"), "\n")
        tabstats::table_default(
            summary_data,
            style_columns = tabstats::td_style(`p-value` = pval_styler)
        )
        cat("\n\n")

        # cli::cli_text("{.field Statistic}            : {round(x$data$stat, 4)}")
        # cli::cli_text("{.field p-value (permutation)}: {round(x$data$p.value, 4)}")
        # cli::cli_text("{.field Permutations}         : {x$data$B}")
        invisible(x)
    }
)
