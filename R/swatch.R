#' Simple colour swatches
#'
#' This function provides a simple visualisation of a colour series as a series
#' of boxes against the specified background colour. If the input has more than
#' one dimension then the boxes will be arranged in a grid (flattening further
#' dimensions after the second).
#' 
#' @param x One or more colours, in any suitable form (see [shade()]).
#' @param bg A background colour.
#' @param border The border colour to draw around each box.
#' @param cex The font size to use for labelling named shades.
#' @param font The font weight to use for labelling named shades, in the sense
#'   of [par()]. The default, 2, corresponds to boldface.
#' @param ... Additional arguments (currently unused).
#' 
#' @examples
#' swatch(c("red", "green", "blue"))
#' @seealso [par()]
#' @author Jon Clayden <code@@clayden.org>
#' @export
swatch <- function (x, bg = "white", border = "grey50", cex = 0.7, font = 2, ...)
{
    shades <- shade(x)
    grid <- .dims(shades, collapse=TRUE)
    if (length(grid) == 1L)
        grid <- c(grid, 1)
    
    width <- 0.9 / (max(grid) + 1)
    gap <- 1 / (max(grid) + 1)
    
    # The first line generates one centre value per location in each dimension
    # The second expands out one x and y position per shade
    stops <- lapply(grid, function(i) gap * ((max(grid) - i) / 2 + seq_len(i)))
    centres <- as.matrix(expand.grid(stops))
    
    oldPars <- par(mai=c(0,0,0,0), bg=bg)
    on.exit(par(oldPars))
    
    devSize <- dev.size()
    devRatio <- devSize[2] / devSize[1]
    
    # Centre coordinates are reversed in the y-axis so that the plot "reads" top-to-bottom
    plot(NA, NA, xlim=c(-0.1,1.1), ylim=0.5+c(-1,1)*devRatio*0.6, xlab="", ylab="", xaxt="n", yaxt="n", bty="n", asp=1)
    rect(centres[,1]-width/2, rev(centres[,2])-width/2, centres[,1]+width/2, rev(centres[,2])+width/2, col=shades, border=border, lwd=2)
    
    dn <- dimnames(shades)
    if (is.null(dn))
        dn <- list(NULL, NULL)
    else if (length(dn) == 1L)
        dn <- c(dn, list(NULL))
    dimLabels <- .names(dn, allowNull=FALSE)
    
    if (!is.null(dn[[1]]))
    {
        text(stops[[1]], min(centres[,2])-width, dn[[1]], col="grey40")
        text(mean(centres[,1]), min(centres[,2])-1.5*width, dimLabels[1], font=2, xpd=TRUE)
    }
    if (!is.null(dn[[2]]))
    {
        text(min(centres[,1])-width, stops[[2]], dn[[2]], col="grey40", srt=90)
        text(min(centres[,1])-1.5*width, mean(centres[,2]), dimLabels[2], font=2, srt=90, xpd=TRUE)
    }
    
    if (.hasNames(shades))
    {
        blackContrast <- contrast(shades, "black")
        whiteContrast <- contrast(shades, "white")
        labelCol <- ifelse(blackContrast > whiteContrast, "black", "white")
        text(centres[,1], rev(centres[,2]), names(shades), col=labelCol, cex=cex, font=font)
    }
}
