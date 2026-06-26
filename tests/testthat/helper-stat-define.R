make_trivial_impl = function() {
    agendas(
        base = baseline(fn = function(.proc, .value = 1) {
            list(value = .value)
        })
    )
}

make_local_model_type = function() {
    S7::new_class(
        paste0("local_model_type_", sample.int(1e6, 1)),
        parent = var_id
    )
}
