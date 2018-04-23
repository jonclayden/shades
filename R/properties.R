.replaceProperty <- function (shades, replacement, space, dim)
{
    shades <- warp(shades, space)
    
    if (is.null(replacement))
        unname(coords(shades)[,dim])
    else
    {
        shape <- .dims(shades)
        
        if (is.numeric(replacement))
        {
            indices <- rep(seq_along(shades), each=length(replacement))
            coords <- coords(shades)[indices,,drop=FALSE]
            coords[,dim] <- rep(replacement, length(shades))
            coords <- .clip(coords, space)
            drop(structure(shade(coords,space=space), dim=c(length(replacement),shape)))
        }
        else
        {
            replacement <- match.fun(replacement)
            temp <- replacement(coords(shades)[1,dim])
            indices <- rep(seq_along(shades), each=length(temp))
            coords <- coords(shades)[indices,,drop=FALSE]
            coords[,dim] <- replacement(coords[,dim])
            coords <- .clip(coords, space)
            drop(structure(shade(coords,space=space), dim=c(length(temp),shape)))
        }
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
#'   values from old ones, notably \code{delta}, which adds its argument, or
#'   \code{scalefac}, which multiplies it.
#' @return Current colour property values, or new colours of class
#'   \code{"shade"}.
#' 
#' @examples
#' saturation(c("papayawhip","lavenderblush","olivedrab"))
#' saturation("papayawhip", 0.7)
#' saturation("papayawhip", delta(0.2))
#' saturation("papayawhip", scalefac(1.5))
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
            alpha <- rep(values, length(shades))
            indices <- rep(seq_along(shades), each=length(values))
            coords <- coords(shades)[indices,,drop=FALSE]
            shades <- drop(structure(shade(coords,space=space(shades),alpha=alpha), dim=c(length(values),.dims(shades))))
        }
        else
        {
            fun <- match.fun(values)
            alpha <- as.numeric(fun(.alpha(shades, allowNull=FALSE)))
            shades <- shade(coords(shades), space=space(shades), alpha=alpha)
        }
        return (shades)
    }
}

#' @rdname properties
#' @export
delta <- function (values)
{
    return (function(x) x+values)
}

#' @rdname properties
#' @export
scalefac <- function (values)
{
    return (function(x) x*values)
}
