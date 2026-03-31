#' 'Variable compared by groups' model mapping
#'
#' Use this when you want to compare `x` by `group`.
#'
#' @export
x_by = function(x, group) {
    args = rlang::enquos(x, group)
    model_id_class(args, "x_by")
}
# x_by = function(x, group) {
#     args = rlang::list2(
#         x = rlang::enquo(x),
#         group = rlang::enquo(group)
#     )
#     model_id_class(args, "x_by")
#     # vctrs::new_vctr(args, class = "x_by")
# }

#' 'Relationship between two variables' model mapping
#'
#' Use this when you want to define the relationship between groups.
#'
#' @export
rel = function(x, resp) {
    args = rlang::list2(
        x = rlang::enquo(x),
        resp = rlang::enquo(resp)
    )
    model_id_class(args, "rel")
    # vctrs::new_vctr(args, class = "rel")
}

#' 'Pairs between variables' model mapping
#'
#' Use this when you want to define the pairwise relationship.
#'
#' @export
pairwise = function(..., direction = "lt") {
    dots = rlang::enquos(...)
    out = list(
        args = list(dots = rlang::expr(c(!!!dots))),
        # args = dots,
        direction = direction
    )
    model_id_class(out, "pairwise")
}

#' @export
model_id_class = function(obj, clss) {
    class(obj) = c(clss, "model_id")
    obj
}
