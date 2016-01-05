#' Colour gradients
#' 
#' This function returns a set of colours interpolating between the specified
#' key colours, equally separated in the specified space.
#' 
#' @param shades One or more colours, in any suitable form (see
#'   \code{\link{shade}}).
#' @param steps A vector giving the number of steps required  between each key
#'   colour. Should have length one less than \code{shades}.
#' @param space The colour space to traverse. Defaults to the current space of
#'   \code{shades}.
#' @return A character vector of class \code{"shade"} containing the gradient
#'   elements in the specified space.
#' 
#' @examples
#' gradient(c("red","blue"), 5)
#' gradient(c("red","blue"), 5, space="LAB")
#' @author Jon Clayden <code@@clayden.org>
#' @export
gradient <- function (shades, steps, space = NULL)
{
    shades <- shade(shades)
    cols <- as(shades, "color")
    
    if (is.null(space))
        space <- attr(shades, "space")
    else if (space != attr(shades,"space"))
        cols <- as(cols, space)
    
    nShades <- length(shades)
    nSteps <- length(steps)
    if (nSteps != 1 && nSteps != nShades - 1)
        warning("Step count will be recycled")
    steps <- rep(steps, length.out=nShades-1)
    
    finalCols <- NULL
    for (i in seq_along(steps))
    {
        diff <- coords(cols)[i+1,] - coords(cols)[i,]
        multipliers <- seq(0, 1, length.out=steps[i])
        for (j in seq_along(multipliers))
            finalCols <- rbind(finalCols, coords(cols)[i,] + multipliers[j] * diff)
    }
    
    return (shade(finalCols, space=space))
}
