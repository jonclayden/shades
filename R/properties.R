.replaceProperty <- function (shades, replacement, space, dim)
{
    shades <- shade(shades)
    cols <- as(shades, space)
    
    if (is.null(replacement))
        coords(cols)[,dim]
    else
    {
        if (is.numeric(replacement))
        {
            indices <- rep(seq_along(shades), each=length(replacement))
            cols@coords <- cols@coords[indices,,drop=FALSE]
            cols@coords[,dim] <- rep(replacement, length(shades))
        }
        else
        {
            replacement <- match.fun(replacement)
            temp <- replacement(cols@coords[1,dim])
            indices <- rep(seq_along(shades), each=length(temp))
            cols@coords <- cols@coords[indices,,drop=FALSE]
            cols@coords[,dim] <- replacement(cols@coords[,dim])
        }
        shade(cols)
    }
}

#' @export
saturation <- function (shades, values = NULL)
{
    .replaceProperty(shades, values, "HSV", 2)
}

#' @export
brightness <- function (shades, values = NULL)
{
    .replaceProperty(shades, values, "HSV", 3)
}

#' @export
hueshift <- function (shades, angles = NULL)
{
    .replaceProperty(shades, function(x) (x+angles) %% 360, "HSV", 1)
}
