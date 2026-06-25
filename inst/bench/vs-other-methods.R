# T-test methods
# Timing several methods (statim vs. base R vs. infer)
# Case: n = 1,000,000

box::use(
    statim[
        define_model, `%by%`, TTEST, prepare,
        state_null, via, conclude
    ],
    infer[specify, hypothesize, calculate, t_test1 = t_test],
    stats[t.test],
    dplyr[slice_sample],
    rstatix[t_test2 = t_test]
)

sleep_large = slice_sample(sleep, n = 1e6L, replace = TRUE, by = group)

bench::mark(
    statim1 = TTEST(extra %by% group, sleep_large),
    statim2 = {
        sleep_large |>
            define_model(extra %by% group) |>
            prepare(TTEST) |>
            state_null(
                MU(extra, group == "2") - 2 * MU(extra, group == "1") == 0
            ) |>
            via("contrast") |>
            conclude()
    },
    statim3 = TTEST(extra ~ group, sleep_large),
    `base R-1` = t.test(extra ~ group, sleep_large),
    `base R-2` = t.test(
        sleep_large$extra[sleep_large$group == "1"],
        sleep_large$extra[sleep_large$group == "2"]
    ),
    infer1 = {
        sleep_large |>
            specify(extra ~ group) |>
            hypothesize(null = "independence") |>
            calculate(stat = "t", order = c("2", "1"))
    },
    infer2 = t_test1(sleep_large, extra ~ group),
    rstatix = t_test2(sleep_large, extra ~ group),
    check = FALSE,
    memory = FALSE
)
