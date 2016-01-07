#' @import grDevices methods colorspace
#' @importFrom graphics par rect
NULL

.spaceFunction <- function (name)
{
    switch(name,
           RGB=colorspace::RGB,
           sRGB=colorspace::sRGB,
           XYZ=colorspace::XYZ,
           LAB=colorspace::LAB,
           polarLAB=colorspace::polarLAB,
           HSV=colorspace::HSV,
           LUV=colorspace::LUV,
           polarLUV=colorspace::polarLUV,
           HLS=colorspace::HLS)
}

#' The shade class
#' 
#' Objects of class \code{"shade"} are simply standard R character vectors
#' representing one or more 8-bit (s)RGB colours in HTML-like hex format, but
#' with extra attributes for internal use.
#' 
#' @param x An R object.
#' @param ... Additional parameters (currently unused).
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
    structure(hex(x,fixup=TRUE), space=class(x), coords=coords(x), class="shade")
}

#' @export
shade.matrix <- function (x, space = "sRGB", ...)
{
    cols <- as(.spaceFunction(space)(x), "sRGB")
    structure(hex(cols,fixup=TRUE), space=space, coords=x, class="shade")
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

setOldClass("shade")

setAs("shade", "color", function(from) {
    return (.spaceFunction(attr(from,"space"))(attr(from,"coords")))
})

setAs("shade", "RGB",      function(from) as(as(from,"color"), "RGB"))
setAs("shade", "sRGB",     function(from) as(as(from,"color"), "sRGB"))
setAs("shade", "XYZ",      function(from) as(as(from,"color"), "XYZ"))
setAs("shade", "LAB",      function(from) as(as(from,"color"), "LAB"))
setAs("shade", "polarLAB", function(from) as(as(from,"color"), "polarLAB"))
setAs("shade", "HSV",      function(from) as(as(from,"color"), "HSV"))
setAs("shade", "HLS",      function(from) as(as(from,"color"), "HLS"))
setAs("shade", "LUV",      function(from) as(as(from,"color"), "LUV"))
setAs("shade", "polarLUV", function(from) as(as(from,"color"), "polarLUV"))
