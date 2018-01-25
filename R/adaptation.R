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

# Derived coefficients from the values above, calculated and cached when the
# package namespace is loaded
.cache <- new.env()

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
#'   blindness). The latter is the rarest in the population. \code{"none"} is
#'   also available, as a pass-through option. Abbrevations, such as the first
#'   letter, may be used, and multiple values are acceptable.
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
dichromat <- function (shades, type = c("protanopic","deuteranopic","tritanopic","none"))
{
    if (missing(type))
        type <- "protanopic"
    else
        type <- match.arg(type, several.ok=TRUE)
    
    shades <- warp(shades, "LMS")
    Q <- coords(shades)
    coords <- do.call(rbind, lapply(type, function(t) {
        Qprime <- Q
        if (t != "none")
        {
            lambda <- switch(t, protanopic=ifelse(Q[,3]/Q[,2] < .cache$Er[1], 575, 475),
                                deuteranopic=ifelse(Q[,3]/Q[,1] < .cache$Er[2], 575, 475),
                                tritanopic=ifelse(Q[,2]/Q[,1] < .cache$Er[3], 660, 485))
            lambda <- as.character(lambda)
            
            if (t == "protanopic")
                Qprime[,1] <- -(.cache$b[lambda]*Q[,2] + .cache$c[lambda]*Q[,3]) / .cache$a[lambda]
            else if (t == "deuteranopic")
                Qprime[,2] <- -(.cache$a[lambda]*Q[,1] + .cache$c[lambda]*Q[,3]) / .cache$b[lambda]
            else
                Qprime[,3] <- -(.cache$a[lambda]*Q[,1] + .cache$b[lambda]*Q[,2]) / .cache$c[lambda]
        }
        return (Qprime)
    }))
    
    indices <- rep(seq_along(shades),each=length(type)) + length(shades) * (seq_along(type)-1)
    coords <- coords[indices,,drop=FALSE]
    
    return (drop(structure(shade(coords,space="LMS"), dim=c(length(type),.dims(shades)))))
}
