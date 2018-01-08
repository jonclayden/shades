# These are approximations to matplotlib's colour scales, with 16 key colours each
.colmaps <- list(magma=c("#000004", "#0B0924", "#20114B", "#3B0F70", "#57157E", "#721F81", "#8C2981", "#A8327D", "#C43C75", "#DE4968", "#F1605D", "#FA7F5E", "#FE9F6D", "#FEBF84", "#FDDEA0", "#FCFDBF"),
                 inferno=c("#000004", "#0C0826", "#240C4F", "#420A68", "#5D126E", "#781C6D", "#932667", "#AE305C", "#C73E4C", "#DD513A", "#ED6925", "#F8850F", "#FCA50A", "#FAC62D", "#F2E661", "#FCFFA4"),
                 plasma=c("#0D0887", "#330597", "#5002A2", "#6A00A8", "#8405A7", "#9C179E", "#B12A90", "#C33D80", "#D35171", "#E16462", "#ED7953", "#F68F44", "#FCA636", "#FEC029", "#F9DC24", "#F0F921"),
                 viridis=c("#440154", "#481A6C", "#472F7D", "#414487", "#39568C", "#31688E", "#2A788E", "#23888E", "#1F988B", "#22A884", "#35B779", "#54C568", "#7AD151", "#A5DB36", "#D2E21B", "#FDE725"))

#' Colour gradients
#' 
#' This function returns a set of colours interpolating between the specified
#' key colours, equally separated in the specified space.
#' 
#' The key colours may be specified explicitly, or else a built-in colour map
#' may be used. The maps available are currently those developed for Python's
#' \code{matplotlib} 2.0 (see the reference URL below), namely \code{"magma"},
#' \code{"inferno"}, \code{"plasma"} and \code{"viridis"}.
#' 
#' @param shades Two or more colours, in any suitable form (see
#'   \code{\link{shade}}), or a named colour map such as \code{"viridis"}.
#' @param steps An integer giving the number of shades required in the palette.
#' @param space The colour space to traverse. Defaults to the current space of
#'   \code{shades}, or \code{"LAB"} for the built-in colour maps.
#' @return A character vector of class \code{"shade"} containing the gradient
#'   elements in the specified space.
#' 
#' @examples
#' gradient(c("red","blue"), 5)
#' gradient(c("red","blue"), 5, space="LAB")
#' gradient("viridis", 5)
#' @references
#' \url{http://bids.github.io/colormap/}
#' @author Jon Clayden <code@@clayden.org>
#' @export
gradient <- function (shades, steps, space = NULL)
{
    if (length(shades) == 1)
    {
        if (is.character(shades) && shades %in% names(.colmaps))
            shades <- warp(shade(.colmaps[[shades]]), "LAB")
        else
            stop("A single-element argument should specify a predefined colour map")
    }
    else
        shades <- shade(shades)
    
    if (is.null(space))
        space <- space(shades)
    else if (space != attr(shades,"space"))
        shades <- warp(shades, space)
    
    nShades <- length(shades)
    locs <- seq(1, nShades, length.out=steps)
    finalCols <- matrix(NA, steps, 3L)
    for (i in seq_along(locs))
    {
        loc <- locs[i]
        fraction <- loc - floor(loc)
        finalCols[i,] <- (1 - fraction) * coords(shades)[floor(loc),] + fraction * coords(shades)[ceiling(loc),]
    }
    
    return (shade(finalCols, space=space))
}
