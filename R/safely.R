safe_eval <- function(...) {
  tryCatch(
    ...,
    error = \(e) e,
    warning = \(w) w
  )
}

is_error <- function(x) {
  inherits(x, "error")
}

is_warning <- function(x) {
  inherits(x, "warning")
}

warn_if <- function(x) {
  if(!is_warning(x))
    return()

  warning(x$message)
}
