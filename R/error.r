define_error <- function(x) {
  define_object(
    message = modify_error_msg(as.character(x)),
    class = 'hero_error'
  )
}

modify_error_msg <- function(x) {
  x <- gsub("Error in eval(x$expr, data, x$env): ", "", x, fixed = TRUE)
  x <- gsub("Error: ", "", x, fixed = TRUE)
  if (grepl('object.*not found', x)) {
    name <- strsplit(x, "'")[[1]][2]
    x <- str_interp('Variable "${name}" not found.')
  }
  x
}

# Determine whether a given object is an error
is_error <- function(x) {
  'hero_error' %in% class(x)
}

#' @export
print.hero_error <- function(x, ...) {
  print(str_interp("Error: ${x$message}"))
}