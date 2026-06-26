glm_def_rel = model_infer_define(
    model_type = rel,
    impl = agendas(
        base = baseline(fn = function(.proc, family = stats::gaussian(), ...) {
            x_data = .proc$x_data
            resp_data = .proc$resp_data
            x_nm = names(x_data)
            resp_nm = names(resp_data)
            df = vctrs::vec_cbind(resp_data, x_data)
            f = stats::reformulate(x_nm, response = resp_nm)
            glm_to_glm_object(stats::glm(f, data = df, family = family, ...))
        })
    )
)

glm_def_formula = model_infer_define(
    model_type = S7::class_formula,
    impl = agendas(
        base = baseline(fn = function(.proc, family = stats::gaussian(), ...) {
            formula = .proc$formula
            data = .proc$data
            glm_to_glm_object(stats::glm(
                formula,
                data = data,
                family = family,
                ...
            ))
        })
    )
)
