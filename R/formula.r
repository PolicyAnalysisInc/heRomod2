#' @export
define_formula <- function(string) {

    # Try to capture the expression
    expr <- try(parse(text = string), silent = TRUE)

    if (inherits(expr, "try-error")) {

        # If the expression can't be parsed, make a formula with
        # an express that generates an error object
        err_msg <- messages$err_syntax
        expr <- parse(text = str_interp("define_error('${err_msg}')"))
        attributes(expr) <- NULL
        res_list <- list(
            text = string,
            expr = expr,
            refs = character()
        )

    } else {

        # Strip attributes
        attributes(expr) <- NULL

        # Build data for heRoFormula object
        res_list <- list(
            text = string,
            expr = expr,
            refs = all.vars(expr, functions = TRUE)
        )

    }
    
    as_hero_formula(res_list)
}

# Evaluate Formula
eval_formula <- function(x, env) {

    if (missing(env)) {
        env <- environment()
    }
  
  # Attempt to evaluate expression
  res <- safe_eval(eval(x$expr, envir = env))
#   if (is_error(res)) {
#     # Check if any of the variables referenced is an error 
#     vars <- x$depends
#     for (i in rev(vars)) {
#       if (i %in% get_names(ns, 'all', keywords = FALSE)) {
#         value <- ns[i]
#         if (is_error(value)) {
#           error_msg <- str_interp('Error in dependency "${i}".')
#           res <- define_error(error_msg)
#         }
#       }
#     }
#   }
  
  # Return the result
  res
}

# Safely evaluate an arbitrary statement and return the result if
# successful or an error object if not.
safe_eval <- function(x) {
  
  # Evaluate the expression
  res <- tryCatch(x, error = function(e) e, silent = TRUE)

  # If an error occurs, create an error message
  if ('error' %in% class(res)) {
    res <- define_error(res)
  }
  
  # Return the result
  res
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

    define_object_(x, c('hero_formula', 'list'))
}

#' @export
print.hero_formula <- function(x, ...) {
  cat(paste0('FORMULA: ', x$text))
}