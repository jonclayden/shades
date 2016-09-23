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
#' Comparison between \code{"shade"} objects \code{x} and \code{y} is achieved
#' by converting \code{y} (the second argument) into the colour space of
#' \code{x} and then comparing coordinates, after any clipping.
#' 
#' @param x,y R objects, or \code{"shade"} objects for methods.
#' @param target,current Shade vectors to compare.
#' @param i An index vector.
#' @param value A vector of replacement colours.
#' @param ... Additional parameters to methods. For \code{c}, any number of
#'   colours in any acceptable form.
#' @return A character vector of class \code{"shade"}, with additional
#'   attributes as follows.
#'     \item{space}{A string naming a color space.}
#'     \item{coords}{A matrix giving colour coordinates in the relevant space,
#'       one colour per row.}
#' 
#' @note When concatenating, shades that are all from the same space will
#'   remain in that space, but shades from different spaces will be warped to
#'   ``XYZ'' space.
#' 
#' @examples
#' s <- shade(c("red", "green", "blue"))
#' s[1]
#' s[1] <- "pink"
#' @author Jon Clayden <code@@clayden.org>
#' @aliases shades
#' @export
shade <- function (x, ...)
{
    UseMethod("shade")
}

#' @rdname shade
#' @export
shade.shade <- function (x, ...)
{
    return (x)
}

#' @rdname shade
#' @export
shade.color <- function (x, ...)
{
    structure(colorspace::hex(x,fixup=TRUE), space=class(x), coords=colorspace::coords(x), class="shade")
}

#' @rdname shade
#' @export
shade.matrix <- function (x, space = "sRGB", ...)
{
    structure(.toHex(x,space), space=space, coords=x, class="shade")
}

#' @rdname shade
#' @export
shade.character <- function (x, ...)
{
    coords <- structure(t(col2rgb(x)/255), dimnames=list(NULL,c("R","G","B")))
    structure(x, space="sRGB", coords=coords, class="shade")
}

#' @rdname shade
#' @export
shade.default <- function (x, ...)
{
    shade.character(as.character(x), ...)
}

#' @rdname shade
#' @export
"[.shade" <- function (x, i)
{
    structure(as.character(x)[i], space=attr(x,"space"), coords=attr(x,"coords")[i,,drop=FALSE], class="shade")
}

#' @rdname shade
#' @export
"[<-.shade" <- function (x, i, value)
{
    replacement <- warp(value, attr(x,"space"))
    attr(x,"coords")[i,] <- attr(replacement,"coords")
    NextMethod("[<-")
}

#' @rdname shade
#' @export
c.shade <- function (...)
{
    shades <- lapply(list(...), shade)
    spaces <- sapply(shades, space)
    
    if (all(spaces == spaces[1]))
        space <- spaces[1]
    else
    {
        space <- "XYZ"
        shades <- lapply(shades, warp, "XYZ")
    }
    
    structure(do.call("c",lapply(shades,as.character)), space=space, coords=do.call("rbind",lapply(shades,coords)), class="shade")
}

#' @rdname shade
#' @export
rep.shade <- function (x, ...)
{
    indices <- rep(seq_along(x), ...)
    structure(as.character(x)[indices], space=attr(x,"space"), coords=attr(x,"coords")[indices,,drop=FALSE], class="shade")
}

#' @rdname shade
#' @export
"==.shade" <- function (x, y)
{
    y <- rep(warp(y,attr(x,"space")), length.out=length(x))
    xCoords <- coords(x)
    yCoords <- coords(y)
    sapply(seq_along(x), function(i) all(xCoords[i,] == yCoords[i,]))
}

#' @rdname shade
#' @export
"!=.shade" <- function (x, y)
{
    return (!`==.shade`(x,y))
}

#' @rdname shade
#' @export
all.equal.shade <- function (target, current, hexonly = FALSE, ...)
{
    if (hexonly)
        all.equal(as.character(target), as.character(current), ...)
    else if (length(target) != length(current))
        paste0("Lengths do not match (", length(target), " and ", length(current), ")")
    else if (all(target == current))
        TRUE
    else
    {
        distances <- sapply(seq_along(target), function(i) distance(target[i],current[i]))
        paste0("Mean colour distance is ", signif(mean(distances,4)))
    }
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
#' Valid names for spaces are currently those supported by the
#' \code{\link{convertColor}} function, namely ``sRGB'', ``Apple RGB'', ``CIE
#' RGB'', ``XYZ'', ``Lab'' and ``Luv''; plus ``RGB'' (which is treated as an
#' alias for ``sRGB'') and ``HSV''. Case is not significant.
#' 
#' @param x An R object which can be coerced to class \code{"shade"}.
#' @param space A string naming the new space.
#' @return A new object of class \code{"shade"}.
#' 
#' @examples
#' warp("red", "HSV")
#' @seealso \code{\link{convertColor}}
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
