ptest_def = test_define(
    model_type = prop,
    impl = agendas(
        base = baseline(
            fn = function(
                .proc,
                .p = 0.5,
                .alt = "two.sided",
                .ci = 0.95,
                .true_p = NULL
            ) {
                res = stats::binom.test(
                    x = .proc$x,
                    n = .proc$n,
                    p = .p,
                    alternative = .alt,
                    conf.level = .ci
                )
                ptest_build(res, .proc, .ci, .p = .true_p %||% .p)
            },
            claim_parser = map_claim(
                .p = function(claim, processed) {
                    claim_scalar(claim, solve_coef = TRUE)$scalar
                },
                .true_p = function(claim, processed) {
                    claim_scalar(claim, solve_coef = FALSE)$scalar
                },
                .alt = function(claim, processed) {
                    switch(
                        claim@op,
                        "==" = ,
                        "!=" = "two.sided",
                        ">=" = ,
                        ">" = "less",
                        "<=" = ,
                        "<" = "greater"
                    )
                }
            )
        ),
        prop = variant(
            fn = function(
                .proc,
                .p = 0.5,
                .alt = "two.sided",
                .ci = 0.95,
                .true_p = NULL,
                correct = TRUE
            ) {
                res = stats::prop.test(
                    x = .proc$x,
                    n = .proc$n,
                    p = .p,
                    alternative = .alt,
                    conf.level = .ci,
                    correct = correct
                )
                ptest_build(res, .proc, .ci, .p = .true_p %||% .p)
            },
            claim_parser = map_claim(
                .p = function(claim, processed) claim_scalar(claim)$scalar,
                .true_p = function(claim, processed) {
                    claim_scalar(claim, solve_coef = FALSE)$scalar
                },
                .alt = function(claim, processed) {
                    switch(
                        claim@op,
                        "==" = ,
                        "!=" = "two.sided",
                        ">=" = ,
                        ">" = "less",
                        "<=" = ,
                        "<" = "greater"
                    )
                }
            )
        )
    ),
    compatible_params = list(PI)
)
