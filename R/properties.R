.replaceProperty <- function (shades, replacement, space, dim)
{
    shades <- warp(shades, space)
    
    if (is.null(replacement))
        structure(coords(shades)[,dim], names=NULL)
    else
    {
        shape <- .dims(shades)
        
        if (is.numeric(replacement))
        {
            indices <- rep(seq_along(shades), each=length(replacement))
            coords <- coords(shades)[indices,,drop=FALSE]
            coords[,dim] <- rep(replacement, length(shades))
            drop(structure(shade(coords,space=space), dim=c(length(replacement),shape)))
        }
        else
        {
            replacement <- match.fun(replacement)
            temp <- replacement(coords(shades)[1,dim])
            indices <- rep(seq_along(shades), each=length(temp))
            coords <- coords(shades)[indices,,drop=FALSE]
            coords[,dim] <- replacement(coords[,dim])
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
#' @param shades One or more colours, in any suitable form (see
#'   \code{\link{shade}}).
#' @param values New values for the property in question. If \code{NULL}, the
#'   current value(s) will be returned.
#' @param angles For \code{hueshift}, the angles (in degrees) by which to
#'   rotate the colour hues.
#' @return Current colour property values, or new colours of class
#'   \code{"shade"}.
#' 
#' @examples
#' saturation(c("papayawhip","lavenderblush","olivedrab"))
#' saturation("papayawhip", 0.7)
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
hueshift <- function (shades, angles = 0)
{
    .replaceProperty(shades, function(x) (x+angles) %% 360, "HSV", 1)
}
