.colmaps <- list()

# These are approximations to matplotlib's colour scales, with 16 key colours each
.colmaps$magma <- matrix(c(0.001462, 0.000466, 0.013866,
                           0.043830, 0.033830, 0.141886,
                           0.123833, 0.067295, 0.295879,
                           0.232077, 0.059889, 0.437695,
                           0.341482, 0.080564, 0.492631,
                           0.445163, 0.122724, 0.506901,
                           0.550287, 0.161158, 0.505719,
                           0.658483, 0.196027, 0.490253,
                           0.767398, 0.233705, 0.457755,
                           0.868793, 0.287728, 0.409303,
                           0.944006, 0.377643, 0.365136,
                           0.981000, 0.498428, 0.369734,
                           0.994738, 0.624350, 0.427397,
                           0.997228, 0.747981, 0.516859,
                           0.993170, 0.870024, 0.626189,
                           0.987053, 0.991438, 0.749504), ncol=3, byrow=TRUE)
.colmaps$inferno=matrix(c(0.001462, 0.000466, 0.013866,
                          0.046915, 0.030324, 0.150164,
                          0.142378, 0.046242, 0.308553,
                          0.258234, 0.038571, 0.406485,
                          0.366529, 0.071579, 0.431994,
                          0.472328, 0.110547, 0.428334,
                          0.578304, 0.148039, 0.404411,
                          0.682656, 0.189501, 0.360757,
                          0.780517, 0.243327, 0.299523,
                          0.865006, 0.316822, 0.226055,
                          0.929644, 0.411479, 0.145367,
                          0.970919, 0.522853, 0.058367,
                          0.987622, 0.645320, 0.039886,
                          0.978806, 0.774545, 0.176037,
                          0.950018, 0.903409, 0.380271,
                          0.988362, 0.998364, 0.644924), ncol=3, byrow=TRUE)
.colmaps$plasma=matrix(c(0.050383, 0.029803, 0.527975,
                         0.200445, 0.017902, 0.593364,
                         0.312543, 0.008239, 0.635700,
                         0.417642, 0.000564, 0.658390,
                         0.517933, 0.021563, 0.654109,
                         0.610667, 0.090204, 0.619951,
                         0.692840, 0.165141, 0.564522,
                         0.764193, 0.240396, 0.502126,
                         0.826588, 0.315714, 0.441316,
                         0.881443, 0.392529, 0.383229,
                         0.928329, 0.472975, 0.326067,
                         0.965024, 0.559118, 0.268513,
                         0.988260, 0.652325, 0.211364,
                         0.994141, 0.753137, 0.161404,
                         0.977995, 0.861432, 0.142808,
                         0.940015, 0.975158, 0.131326), ncol=3, byrow=TRUE)
.colmaps$viridis=matrix(c(0.267004, 0.004874, 0.329415,
                          0.282656, 0.100196, 0.422160,
                          0.277134, 0.185228, 0.489898,
                          0.253935, 0.265254, 0.529983,
                          0.221989, 0.339161, 0.548752,
                          0.190631, 0.407061, 0.556089,
                          0.163625, 0.471133, 0.558148,
                          0.139147, 0.533812, 0.555298,
                          0.120565, 0.596422, 0.543611,
                          0.134692, 0.658636, 0.517649,
                          0.208030, 0.718701, 0.472873,
                          0.327796, 0.773980, 0.406640,
                          0.477504, 0.821444, 0.318195,
                          0.647257, 0.858400, 0.209861,
                          0.824940, 0.884720, 0.106217,
                          0.993248, 0.906157, 0.143936), ncol=3, byrow=TRUE)

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
