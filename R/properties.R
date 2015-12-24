.replaceProperty <- function (shades, replacement, space, dim)
{
    shades <- shade(shades)
    cols <- as(shades, space)
    
    if (is.null(replacement))
        coords(cols)[,dim]
    else
    {
        indices <- rep(seq_along(shades), each=length(replacement))
        cols@coords <- cols@coords[indices,]
        if (is.numeric(replacement))
            cols@coords[,dim] <- rep(replacement, length(shades))
        else
        {
            replacement <- match.fun(replacement)
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

