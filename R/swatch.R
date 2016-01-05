#' Simple colour swatches
#'
#' This function provides a simple visualisation of a colour series as a series
#' of boxes against the specified background colour.
#' 
#' @param x One or more colours, in any suitable form (see
#'   \code{\link{shade}}).
#' @param bg A background colour.
#' @param ... Additional arguments (currently unused).
#' 
#' @author Jon Clayden <code@@clayden.org>
#' @export
swatch <- function (x, bg = "white", ...)
{
    shades <- shade(x)
    width <- 0.9 / (length(shades)+1)
    centres <- 1:length(shades) / (length(shades)+1)
    
    oldPars <- par(mai=c(0,0,0,0), bg=bg)
    
    devSize <- dev.size()
    devRatio <- devSize[2] / devSize[1]
    
    plot(NA, NA, xlim=c(-0.1,1.1), ylim=0.5+c(-1,1)*devRatio*0.6, xlab="", ylab="", xaxt="n", yaxt="n", bty="n", asp=1)
    rect(centres-width/2, 0.5-width/2, centres+width/2, 0.5+width/2, col=shades, border=NA)
    
    par(oldPars)
}
