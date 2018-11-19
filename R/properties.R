.replaceProperty <- function (shades, replacement, space, dim)
{
    UseMethod(".replaceProperty")
}

.replaceProperty.function <- function (shades, replacement, space, dim)
{
    function (...) .replaceProperty(shades(...), replacement, space, dim)
}

# .replaceProperty.ggproto_method <- .replaceProperty.function

.replaceProperty.Scale <- function (shades, replacement, space, dim)
{
    ggplot2::ggproto(NULL, shades, palette=function(self,...) {
        colours <- ggplot2::ggproto_parent(shades, self)$palette(...)
        .replaceProperty(colours, replacement, space, dim)
    })
}

.replaceProperty.default <- function (shades, replacement, space, dim) 
{
    shades <- warp(shades, space)
    
    if (is.null(replacement))
        return (unname(coords(shades)[,dim]))
    else
    {
        if (is.numeric(replacement))
        {
            arity <- length(replacement)
            replacement <- rep(replacement, length(shades))
        }
        else
        {
            fun <- match.fun(replacement)
            arity <- length(fun(coords(shades)[1,dim]))
            replacement <- fun(coords(shades)[,dim])
        }
        
        indices <- rep(seq_along(shades), each=arity)
        coords <- coords(shades)[indices,,drop=FALSE]
        coords[,dim] <- replacement
        coords <- .clip(coords, space)
        alpha <- .alpha(shades, allowNull=FALSE)[indices]
        return (drop(structure(shade(coords,space=space,alpha=alpha), dim=c(arity,.dims(shades)))))
    }
}

#' Query or change colour properties
#' 
#' These functions obtain the value of a colour property, or modify it. They
#' will convert between colour spaces as required, but the RGB representation
#' will be appropriately updated in the result.
#' 
#' Brightness and lightness differ technically, in the sense that one is
#' absolute and the other is relative. Intuitively, a grey square on white
#' paper is brighter under bright sunlight than in a dark room, but its
#' lightness (relative to the white around it) is constant between conditions.
#' In these functions, brightness is ``value'' in HSV space and is between 0
#' and 1, while lightness is defined in Lab space and is between 0 and 100.
#' Saturation and chroma are also related. Hue is defined in HSV space, with
#' red at 0º (and 360º), which is generally the most familiar parameterisation.
#' 
#' @param shades One or more colours, in any suitable form (see
#'   \code{\link{shade}}).
#' @param values New values for the property in question. If \code{NULL}, the
#'   current value(s) will be returned. May also be a function computing new
#'   values from old ones, such as \code{delta}, which adds its argument, or
#'   \code{scalefac}, which multiplies it.
#' @param ... Arguments to replacement functions \code{delta}, \code{scalefac}
#'   and \code{recycle}, which will be concatenated.
#' @return Current colour property values, or new colours of class
#'   \code{"shade"}.
#' 
#' @note The colour property functions are vectorised over both of their
#'   arguments, such that the dimensions of the result will be
#'   \code{c(length(values),dim(shades))}. However, the \code{recycle} function
#'   can be used to suppress the usual dimensional expansion, and instead
#'   follow R's standard recycling rule.
#' 
#' @examples
#' saturation(c("papayawhip","lavenderblush","olivedrab"))
#' 
#' saturation("papayawhip", 0.7)
#' saturation("papayawhip", delta(0.2))
#' saturation("papayawhip", scalefac(1.5))
#' 
#' saturation(c("red","green"), c(0.4,0.6))
#' saturation(c("red","green"), recycle(0.4,0.6))
#' @author Jon Clayden <code@@clayden.org>
#' @rdname properties
#' @export
saturation <- function (shades, values = NULL)
{
    .replaceProperty(shades, values, "HSV", 2)
}

#' @rdname properties
#' @export
brightness <- function (shades, values = NULL)
{
    .replaceProperty(shades, values, "HSV", 3)
}

#' @rdname properties
#' @export
lightness <- function (shades, values = NULL)
{
    .replaceProperty(shades, values, "Lab", 1)
}

#' @rdname properties
#' @export
chroma <- function (shades, values = NULL)
{
    .replaceProperty(shades, values, "LCh", 2)
}

#' @rdname properties
#' @export
hue <- function (shades, values = NULL)
{
    .replaceProperty(shades, values, "HSV", 1)
}

#' @rdname properties
#' @export
opacity <- function (shades, values = NULL)
{
    shades <- shade(shades)
    
    if (is.null(values))
        return (.alpha(shades, allowNull=FALSE))
    else
    {
        if (is.numeric(values))
        {
            arity <- length(values)
            values <- rep(values, length(shades))
        }
        else
        {
            fun <- match.fun(values)
            arity <- length(fun(.alpha(shades,allowNull=FALSE)[1]))
            values <- fun(.alpha(shades, allowNull=FALSE))
        }
        
        indices <- rep(seq_along(shades), each=arity)
        coords <- coords(shades)[indices,,drop=FALSE]
        return (drop(structure(shade(coords,space=space(shades),alpha=values), dim=c(arity,.dims(shades)))))
    }
}

#' @rdname properties
#' @export
delta <- function (...)
{
    values <- as.numeric(c(...))
    return (function(x) x+values)
}

#' @rdname properties
#' @export
scalefac <- function (...)
{
    values <- as.numeric(c(...))
    return (function(x) x*values)
}

#' @rdname properties
#' @export
recycle <- function (...)
{
    values <- as.numeric(c(...))
    return (function(x) rep(values,length.out=length(x)))
}
