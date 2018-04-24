# Linearised transformation matrices
.bradfordXYZtoLMS <- matrix(c(0.8951, -0.7502, 0.0389, 0.2664, 1.7135, -0.0685, -0.1614, 0.0367, 1.0296), 3, 3)
.bradfordLMStoXYZ <- solve(.bradfordXYZtoLMS)

# Standard and additional colour space converters
.converters <- list(rgb="sRGB", srgb="sRGB", xyz="XYZ", "apple rgb"="Apple RGB", "cie rgb"="CIE RGB", lab="Lab", luv="Luv")

.converters$hsv <- colorConverter(
    toXYZ = function (hsv, ...) {
        rgb <- drop(col2rgb(hsv((hsv[1] %% 360)/360, hsv[2], hsv[3])) / 255)
        colorspaces$sRGB$toXYZ(rgb, ...)
    },
    fromXYZ = function (xyz, ...) {
        # This rounding operation mirrors convertColor(), and avoids numerical variability between platforms
        rgb <- round(colorspaces$sRGB$fromXYZ(xyz, ...), 5)
        rgb[rgb < 0] <- 0
        rgb[rgb > 1] <- 1
        hsv <- drop(rgb2hsv(t(rgb), maxColorValue=1))
        hsv[1] <- (hsv[1] * 360) %% 360
        structure(hsv, names=c("H","S","V"))
    },
    name = "HSV")

.converters$lms <- colorConverter(
    toXYZ = function (lms, ...) { .bradfordLMStoXYZ %*% lms },
    fromXYZ = function (xyz, ...) { structure(.bradfordXYZtoLMS %*% xyz, names=c("L","M","S")) },
    name = "LMS")

.converters$lch <- colorConverter(
    toXYZ = function (lch, ...) {
        angle <- lch[3] / 180 * pi
        lab <- c(lch[1], lch[2] * cos(angle), lch[2] * sin(angle))
        colorspaces$Lab$toXYZ(lab, ...)
    },
    fromXYZ = function (xyz, ...) {
        lab <- colorspaces$Lab$fromXYZ(xyz, ...)
        lch <- c(lab[1], sqrt(lab[2]^2 + lab[3]^2), atan2(lab[3],lab[2]) / pi * 180)
        lch[3] <- lch[3] %% 360
        structure(lch, names=c("L","C","h"))
    },
    name = "LCh")

.toHex <- function (coords, space, alpha = NULL)
{
    space <- tolower(space)
    
    if (!identical(.converters[[space]], "sRGB"))
        coords <- convertColor(coords, .converters[[space]], "sRGB")
    
    if (is.null(alpha) || all(alpha == 1))
        return (rgb(coords[,1], coords[,2], coords[,3], maxColorValue=1))
    else
        return (rgb(coords[,1], coords[,2], coords[,3], pmax(0,pmin(1,alpha)), maxColorValue=1))
}

.clip <- function (coords, space)
{
    if (grepl("rgb$", tolower(space)))
    {
        coords[coords < 0] <- 0
        coords[coords > 1] <- 1
    }
    else if (tolower(space) == "hsv")
    {
        temp <- coords[,1] %% 360
        coords[coords < 0] <- 0
        coords[coords > 1] <- 1
        coords[,1] <- temp
    }
    
    return (coords)
}

.dims <- function (x, collapse = FALSE)
{
    if (is.null(dim(x)))
        return (length(x))
    else if (length(dim(x)) > 2 && collapse)
        return (c(dim(x)[1], prod(dim(x)[-1])))
    else
        return (dim(x))
}

.alpha <- function (x, ..., allowNull = TRUE)
{
    if (!missing(..1))
    {
        elements <- lapply(list(x,...), .alpha, allowNull=FALSE)
        result <- do.call("c", elements)
    }
    else
    {
        if (!is.null(attr(x, "alpha")))
            result <- pmax(0, pmin(1, attr(x,"alpha")))
        else if (any(grepl("#[0-9A-Fa-f]{8}", as.character(x), perl=TRUE)))
            result <- unname(col2rgb(as.character(x), alpha=TRUE)["alpha",] / 255)
        else
            result <- rep(1, length(x))
    }
    
    if (allowNull && all(result == 1))
        return (NULL)
    else
        return (result)
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
#' @param space For a matrix, the space in which coordinates are being
#'   provided.
#' @param alpha For a matrix, an associated vector of opacity values between 0
#'   and 1, if required.
#' @param target,current Shade vectors to compare.
#' @param i An index vector.
#' @param value A vector of replacement colours.
#' @param hexonly If \code{TRUE}, compare only on the basis of the hex strings.
#'   Otherwise test for equal coordinates.
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
    hex <- colorspace::hex(x, fixup=TRUE)
    structure(hex, space=class(x), coords=colorspace::coords(x), alpha=.alpha(hex), class="shade")
}

#' @rdname shade
#' @export
shade.matrix <- function (x, space = "sRGB", alpha = NULL, ...)
{
    hex <- .toHex(x, space, alpha)
    structure(hex, space=space, coords=x, alpha=.alpha(hex), class="shade")
}

#' @rdname shade
#' @export
shade.character <- function (x, ...)
{
    if (length(x) == 0)
        stop("Colour vector must not be empty")
    coords <- structure(t(col2rgb(x)/255), dimnames=list(NULL,c("R","G","B")))
    structure(x, space="sRGB", coords=coords, alpha=.alpha(x), class="shade")
}

#' @rdname shade
#' @export
shade.default <- function (x, ...)
{
    if (missing(x))
        stop("Colour vector must not be empty")
    shade.character(as.character(x), ...)
}

#' @rdname shade
#' @export
print.shade <- function (x, ...)
{
    len <- length(x)
    hasAlpha <- !is.null(attr(x, "alpha"))
    cat(paste0(" ", len, ifelse(len==1," shade"," shades"), " in ", space(x), " space, ", ifelse(hasAlpha,"with","without"), " transparency\n"))
    print(structure(x, space=NULL, coords=NULL, alpha=NULL, class=NULL), quote=FALSE)
}

#' @rdname shade
#' @export
"[.shade" <- function (x, i)
{
    structure(as.character(x)[i], space=attr(x,"space"), coords=attr(x,"coords")[i,,drop=FALSE], alpha=attr(x,"alpha")[i], class="shade")
}

#' @rdname shade
#' @export
"[<-.shade" <- function (x, i, value)
{
    replacement <- warp(value, attr(x,"space"))
    attr(x,"coords")[i,] <- attr(replacement,"coords")
    
    alpha <- .alpha(x, allowNull=FALSE)
    alpha[i] <- .alpha(value, allowNull=FALSE)
    if (all(alpha == 1))
        attr(x, "alpha") <- NULL
    else
        attr(x, "alpha") <- alpha
    
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
    
    structure(do.call("c",lapply(shades,as.character)), space=space, coords=do.call("rbind",lapply(shades,coords)), alpha=do.call(".alpha",shades), class="shade")
}

#' @rdname shade
#' @export
rep.shade <- function (x, ...)
{
    indices <- rep(seq_along(x), ...)
    structure(as.character(x)[indices], space=attr(x,"space"), coords=attr(x,"coords")[indices,,drop=FALSE], alpha=attr(x,"alpha")[indices], class="shade")
}

#' @rdname shade
#' @export
rev.shade <- function (x)
{
    indices <- rev(seq_along(x))
    structure(as.character(x)[indices], space=attr(x,"space"), coords=attr(x,"coords")[indices,,drop=FALSE], alpha=attr(x,"alpha")[indices], class="shade")
}

#' @rdname shade
#' @export
"==.shade" <- function (x, y)
{
    y <- rep(warp(y,attr(x,"space")), length.out=length(x))
    xCoords <- coords(x)
    yCoords <- coords(y)
    coordsAgree <- sapply(seq_along(x), function(i) all(xCoords[i,] == yCoords[i,]))
    alphaAgrees <- .alpha(x, allowNull=FALSE) == .alpha(y, allowNull=FALSE)
    return (coordsAgree & alphaAgrees)
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
    else if (!all(.alpha(target,allowNull=FALSE) == .alpha(current,allowNull=FALSE)))
        paste0("Alpha values do not match (mean absolute difference is ", signif(mean(abs(.alpha(target,allowNull=FALSE) - .alpha(current,allowNull=FALSE))),4), ")")
    else
    {
        target <- warp(target, space(current))
        result <- all.equal(coords(target), coords(current), ...)
        if (isTRUE(result))
            return (TRUE)
        else
        {
            distances <- sapply(seq_along(target), function(i) distance(target[i],current[i]))
            paste0("Mean colour distance is ", signif(mean(distances,4)))
        }
    }
}

#' Retrieve the space of a colour vector
#' 
#' This function retrieves the colour space in which its argument is currently
#' defined.
#' 
#' @param x An R object.
#' @param ... Additional arguments to methods.
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
#' @param x An R object.
#' @param ... Additional arguments to methods.
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
#' alias for ``sRGB''), ``HSV'', ``LCh'' and ``LMS''. Case is not significant.
#' 
#' @param x An R object which can be coerced to class \code{"shade"}.
#' @param space A string naming the new space.
#' @return A new object of class \code{"shade"}.
#' 
#' @note LMS space, used for chromatic adaptation and simulating colour
#'   blindness, is not uniquely defined. Here we use the (linearised) Bradford
#'   transform, obtained by Lam (1985) and used widely in ICC colour profiles
#'   and elsewhere, to transform to and from CIE XYZ space.
#'   
#'   R uses the D65 standard illuminant as the reference white for the ``Lab''
#'   and ``Luv'' spaces.
#' 
#' @examples
#' warp("red", "HSV")
#' @references
#' Lam, K.M. (1985). Metamerism and colour constancy. PhD thesis, University of
#' Bradford.
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
    
    coords <- convertColor(attr(x,"coords"), .converters[[sourceSpace]], .converters[[targetSpace]])
    alpha <- .alpha(x)
    
    return (structure(.toHex(coords,targetSpace,alpha), dim=dim(x), space=space, coords=coords, alpha=alpha, class="shade"))
}
