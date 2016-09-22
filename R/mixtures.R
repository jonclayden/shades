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

.mix <- function (base, mixer, op, amount = 1, space = NULL)
{
    op <- match.fun(op)
    
    if (is.null(space))
        space <- space(base)
    
    amount <- rep(amount, length.out=length(mixer))
    
    base <- warp(base, space)
    mixer <- warp(mixer, space)
    
    baseIndices <- rep(seq_along(base), each=length(mixer))
    mixerIndices <- rep(seq_along(mixer), length(base))
    
    coords <- sapply(1:3, function(i) op(coords(base)[baseIndices,i], amount[mixerIndices]*coords(mixer)[mixerIndices,i]))
    if (!is.matrix(coords))
        coords <- matrix(coords, nrow=1)
    
    shade(.clip(coords,space), space=space)
}

#' Complementary colours
#' 
#' This function returns the complement of its argument, the "opposite" colours
#' in the specified space.
#' 
#' @param shades One or more colours, in any suitable form (see
#'   \code{\link{shade}}).
#' @param space The space in which to take the complement.
#' @return New colours of class \code{"shade"}.
#' 
#' @examples
#' complement("cyan")
#' complement("cyan", space="HSV")
#' complement("cyan", space="Lab")
#' @export
complement <- function (shades, space = NULL)
{
    if (is.null(space))
        space <- space(shades)
    
    if (tolower(space) == "hsv")
        hueshift(shades, 180)
    else
    {
        white <- warp("white", space=space)
        .mix(white, shades, "-", space=space)
    }
}

#' Colour mixtures
#' 
#' These functions allow colours to be mixed in any colour space, either
#' additively (like light) or subtractively (like paint). The infix form
#' \code{\%.)\%} is an alternative for \code{addmix}, and \code{\%_/\%} for
#' \code{submix}, with the mixing amount being fixed to 1 in these cases.
#' 
#' @param base,X A vector of base colours.
#' @param mixer,Y A vector of colours to mix in.
#' @param amount The amount of each colour to mix in, relative to the amount
#'   of the base. This will be recycled to the length of \code{mixer}.
#' @param space A string giving the space in which to perform the mixing, or
#'   \code{NULL}. In the latter case, the space of \code{base} will be used.
#' 
#' @examples
#' addmix(c("red","green","blue"), "red")
#' submix(c("cyan","magenta","yellow"), "cyan")
#' @author Jon Clayden <code@@clayden.org>
#' @rdname mixtures
#' @export
addmix <- function (base, mixer, amount = 1, space = NULL)
{
    .mix(base, mixer, "+", amount, space)
}

#' @rdname mixtures
#' @export
submix <- function (base, mixer, amount = 1, space = NULL)
{
    complement(.mix(complement(base,space), complement(mixer,space), "+", amount, space))
}

#' @rdname mixtures
#' @export
"%.)%" <- function (X, Y)
{
    .mix(X, Y, "+")
}

#' @rdname mixtures
#' @export
"%_/%" <- function (X, Y)
{
    .mix(X, Y, "-")
}
