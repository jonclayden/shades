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
    grid <- .dims(shades, collapse=TRUE)
    if (length(grid) == 1)
        grid <- c(grid, 1)
    
    width <- 0.9 / (max(grid) + 1)
    gap <- 1 / (max(grid) + 1)
    
    # The first line generates one centre value per location in each dimension
    # The second expands out one x and y position per shade
    centres <- lapply(grid, function(i) gap * ((max(grid) - i) / 2 + seq_len(i)))
    centres <- as.matrix(expand.grid(centres))
    
    oldPars <- par(mai=c(0,0,0,0), bg=bg)
    on.exit(par(oldPars))
    
    devSize <- dev.size()
    devRatio <- devSize[2] / devSize[1]
    
    # Centre coordinates are reversed in the y-axis so that the plot "reads" top-to-bottom
    plot(NA, NA, xlim=c(-0.1,1.1), ylim=0.5+c(-1,1)*devRatio*0.6, xlab="", ylab="", xaxt="n", yaxt="n", bty="n", asp=1)
    rect(centres[,1]-width/2, rev(centres[,2])-width/2, centres[,1]+width/2, rev(centres[,2])+width/2, col=shades, border="grey50", lwd=2)
}
