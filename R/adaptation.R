# The Brettel algorithm calls for "the brightest possible metamer of an equal-
# energy stimulus". The equal-energy stimulus, CIE illuminant E, has equal
# values for each coordinate in XYZ space. 0.8388 is the largest equal-energy
# value (to 4 d.p.) within the sRGB gamut, according to my experimentation.
.equalEnergy <- matrix(0.8388, nrow=1, ncol=3)

# XYZ space coordinates for the specified wavelengths, using the standard (CIE
# 1931) observer. This is a small subset of the values available from
# http://files.cie.co.at/204.xls.
.standardObserver <- list("475"=matrix(c(0.1421,0.1126,1.0419), nrow=1),
                          "485"=matrix(c(0.05795,0.1693,0.6162), nrow=1),
                          "575"=matrix(c(0.8425,0.9154,0.0018), nrow=1),
                          "660"=matrix(c(0.1649,0.061,0), nrow=1))

#' Simulate colour appearance for dichromats
#' 
#' This functions manipulates colours to simulate the effects of different
#' kinds of colour blindness, and specifically dichromacy, in which only two of
#' the usual three types of photoreceptors are present. There are three types,
#' corresponding to the loss of red, green or blue photoreceptors.
#' 
#' @param shades One or more colours, in any suitable form (see
#'   \code{\link{shade}}).
#' @param type The type of colour vision deficiency to simulate: protanopia
#'   (red blindness), deuteranopia (green blindness) or tritanopia (blue
#'   blindness). The latter is the rarest in the population. Abbrevations,
#'   such as the first letter, may be used.
#' @return New colours of class \code{"shade"} in LMS space, representing
#'   projections of the original shades onto a submanifold appropriate to the
#'   type of dichromacy being simulated.
#' 
#' @examples
#' dichromat(c("red", "green", "blue"))
#' @references
#' Brettel, H., Viénot, F. and Mollon, J.D. (1997). Computerized simulation of
#' color appearance for dichromats. Journal of the Optical Society of America A
#' 14(10):2647-2655.
#' @author Jon Clayden <code@@clayden.org>
#' @export
dichromat <- function (shades, type = c("protanopic","deuteranopic","tritanopic"))
{
    type <- match.arg(type)
    
    Q <- coords(warp(shades, "LMS"))
    E <- coords(warp(shade(.equalEnergy,space="XYZ"), "LMS"))
    lambda <- switch(type, protanopic=ifelse(Q[,3]/Q[,2] < E[,3]/E[,2], 575, 475),
                           deuteranopic=ifelse(Q[,3]/Q[,1] < E[,3]/E[,1], 575, 475),
                           tritanopic=ifelse(Q[,2]/Q[,1] < E[,2]/E[,1], 660, 485))
    lambda <- as.character(lambda)
    
    abc <- sapply(.standardObserver, function(xyz) {
        A <- coords(warp(shade(xyz,space="XYZ"), "LMS"))
        a <- E[,2] * A[,3] - E[,3] * A[,2]
        b <- E[,3] * A[,1] - E[,1] * A[,3]
        c <- E[,1] * A[,2] - E[,2] * A[,1]
        c(a, b, c)
    })
    rownames(abc) <- c("a", "b", "c")
    
    Qprime <- Q
    if (type == "protanopic")
        Qprime[,1] <- -(abc["b",lambda]*Q[,2] + abc["c",lambda]*Q[,3]) / abc["a",lambda]
    else if (type == "deuteranopic")
        Qprime[,2] <- -(abc["a",lambda]*Q[,1] + abc["c",lambda]*Q[,3]) / abc["b",lambda]
    else
        Qprime[,3] <- -(abc["a",lambda]*Q[,1] + abc["b",lambda]*Q[,2]) / abc["c",lambda]
    
    return (shade(Qprime, space="LMS"))
}
