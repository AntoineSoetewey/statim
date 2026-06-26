# "tidy" t-test results
# Timing several methods
# Case: n = 1,000,000

box::use(
    statim[
        define_model,
        `%by%`,
        TTEST,
        prepare_test,
        state_null,
        via,
        conclude,
        auto_tidy,
        tidy
    ],
    dplyr[slice_sample]
)

sleep_large = slice_sample(sleep, n = 1e6L, replace = TRUE, by = group)

bench::mark(
    statim1 = auto_tidy(TTEST(extra %by% group, sleep_large)@data),
    statim2 = {
        sleep_large |>
            define_model(extra %by% group) |>
            prepare_test(TTEST) |>
            state_null(
                MU(extra, group == "2") - 2 * MU(extra, group == "1") == 0
            ) |>
            via("contrast") |>
            conclude() |>
            tidy()
    },
    check = FALSE,
    memory = FALSE
)
