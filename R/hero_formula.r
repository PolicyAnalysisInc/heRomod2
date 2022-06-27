#' @export
define_formula <- function(string) {
    # Try to capture the expression
    expr <- parse(text = string)

    # Build data for heRoFormula object
    res_list <- list(
        text = string,
        expr = expr,
        refs = all.vars(expr, functions = T)
    )
    
    as_hero_formula(res_list)
}

#' @export
set_deps <- function(x, deps) {
    x$deps <- deps
    class(x) <- c('hero_formula_resolved', 'hero_formula', 'list')
    x
}

#' @export
as_hero_formula <- function(x) {
    UseMethod('as_hero_formula', x)
}

#' @export
as_hero_formula.list <- function(x) {

    # Check for essential fields then set class property.
    props_to_check <- c('text', 'expr', 'refs')
    for (prop in props_to_check) {
        if (is.null(x[[prop]])) stop(paste0('Property "', prop, '" was missing.'))
    }
    class(x) <- c('hero_formula', 'list')

    x
}

#' @export
print.hero_formula <- function(x, ...) {
  cat(paste0('FORMULA: ', x$text))
}