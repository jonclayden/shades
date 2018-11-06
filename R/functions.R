
#' @export
lightness <- function (shades, values = NULL) UseMethod("lightness")

#' @export
lightness.function <- function(shades, values = NULL) {
  orig_call <- match.call(expand.dots = FALSE)
  orig_call[[1]] <- quote(lightness)
  palette <- shades
  palette_q <- substitute(shades)
  orig_env <- parent.frame()
  
  wrapper <- function (...) {
    inner_call <- match.call(expand.dots = FALSE)
    inner_call[[1]] <- palette_q
    result <- eval(inner_call, envir = orig_env)
    orig_call$shades <- result
    eval(orig_call)
  }
  
  formals(wrapper) <- formals(palette)
  class(wrapper) <- class(palette)
   
  return(wrapper)
}

#' @export
lightness.ggproto_method <- lightness.function

#' @export
lightness.Scale <- function (shades, values = NULL) {
  orig_call <- match.call(expand.dots = FALSE)
  orig_call[[1]] <- quote(lightness)
  
  new_scale <- ggproto(NULL, shades,
        palette = function (self, n) {
          res <- ggproto_parent(shades, self)$palette(n)
          lightness(res, values)
        })

  new_scale
}