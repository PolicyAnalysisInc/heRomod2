define_object <- function(..., class) {
  define_object_(list(...), class)
}

define_object_ <- function(obj, class) {
  class(obj) <- class
  obj
}