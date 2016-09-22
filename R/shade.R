#' @import grDevices
#' @importFrom graphics par rect
NULL

.lowerCaseToR <- list(rgb="sRGB", srgb="sRGB", hsv="sRGB", xyz="XYZ", "apple rgb"="Apple RGB", "cie rgb"="CIE RGB", lab="Lab", luv="Luv")

.toHex <- function (coords, space)
{
    space <- tolower(space)
    
    if (space == "hsv")
        coords <- t(col2rgb(hsv((coords[,1] %% 360)/360, coords[,2], coords[,3])) / 255)
    else if (.lowerCaseToR[[space]] != "sRGB")
        coords <- convertColor(coords, .lowerCaseToR[[space]], "sRGB")
    
    return (rgb(coords[,1], coords[,2], coords[,3], maxColorValue=1))
}

#' The shade class
#' 
#' Objects of class \code{"shade"} are simply standard R character vectors
#' representing one or more 8-bit (s)RGB colours in CSS-like hex format, but
#' with extra attributes giving the current colour space and coordinates.
#' 
#' @param x An R object.
#' @param ... Additional parameters to methods.
#' @return A character vector of class \code{"shade"}, with additional
#'   attributes as follows.
#'     \item{space}{A string naming a color space.}
#'     \item{coords}{A matrix giving colour coordinates in the relevant space,
#'       one colour per row.}
#' 
#' @author Jon Clayden <code@@clayden.org>
#' @aliases shades
#' @export
shade <- function (x, ...)
{
    UseMethod("shade")
}

#' @export
shade.shade <- function (x, ...)
{
    return (x)
}

#' @export
shade.color <- function (x, ...)
{
    structure(colorspace::hex(x,fixup=TRUE), space=class(x), coords=colorspace::coords(x), class="shade")
}

#' @export
shade.matrix <- function (x, space = "sRGB", ...)
{
    structure(.toHex(x,space), space=space, coords=x, class="shade")
}

#' @export
shade.character <- function (x, ...)
{
    coords <- structure(t(col2rgb(x)/255), dimnames=list(NULL,c("R","G","B")))
    structure(x, space="sRGB", coords=coords, class="shade")
}

#' @export
shade.default <- function (x, ...)
{
    shade.character(as.character(x), ...)
}

#' Retrieve the space of a colour vector
#' 
#' This function retrieves the colour space in which its argument is currently
#' defined.
#' 
#' @param An R object.
#' @return A string naming a colour space.
#'
#' @examples
#' space("red")
#' @author Jon Clayden <code@@clayden.org>
#' @export
space <- function (x, ...)
{
    UseMethod("space")
}

#' @export
space.shade <- function (x, ...)
{
    attr(x, "space")
}

#' @export
space.default <- function (x, ...)
{
    space.shade(shade(x, ...))
}

#' Retrieve the coordinates of a colour vector
#' 
#' This function retrieves the coordinates of a colour vector's elements,
#' within whatever space it is currently defined.
#' 
#' @param An R object.
#' @return A matrix giving colour coordinates in the relevant space, one colour
#'   per row. Columns are typically named.
#'
#' @examples
#' coords("red")
#' @author Jon Clayden <code@@clayden.org>
#' @export
coords <- function (x, ...)
{
    UseMethod("coords")
}

#' @export
coords.shade <- function (x, ...)
{
    attr(x, "coords")
}

#' @export
coords.default <- function (x, ...)
{
    coords.shade(shade(x, ...))
}

#' Shift colours between spaces
#' 
#' This function shifts the current colour space of its arguments to the
#' specified space, returning a new object of class \code{"shade"}.
#' 
#' @param x An R object which can be coerced to class \code{"shade"}.
#' @param space A string naming the new space.
#' @return A new object of class \code{"shade"}.
#' 
#' @examples
#' warp("red", "HSV")
#' @author Jon Clayden <code@@clayden.org>
#' @export
warp <- function (x, space)
{
    x <- shade(x)
    sourceSpace <- tolower(attr(x, "space"))
    targetSpace <- tolower(space)
    
    if (sourceSpace == targetSpace)
        return (x)
    
    coords <- attr(x, "coords")
    
    if (sourceSpace == "hsv")
        coords <- t(col2rgb(hsv((coords[,1] %% 360)/360, coords[,2], coords[,3])) / 255)
    
    coords <- convertColor(coords, .lowerCaseToR[[sourceSpace]], .lowerCaseToR[[targetSpace]])
    
    if (targetSpace == "hsv")
    {
        coords <- t(rgb2hsv(t(coords), maxColorValue=1))
        coords[,1] <- coords[,1] * 360
    }
    
    return (structure(.toHex(coords,targetSpace), space=space, coords=coords, class="shade"))
}
