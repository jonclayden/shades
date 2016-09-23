#' Colour distance
#' 
#' This function calculates a distance measure that aims to quantify the
#' perceptual difference between a vector of colours and a reference colour.
#' The measure in question is the CIE Delta E (2000), which is calculated based
#' on colour coordinates in Lab space.
#' 
#' @param shades One or more colours, in any suitable form (see
#'   \code{\link{shade}}).
#' @param reference A single reference colour.
#' @return A numeric vector of distances.
#' 
#' @examples
#' distance(c("red","green","blue"), "red")
#' @references
#' \url{http://www.brucelindbloom.com/index.html?Eqn_DeltaE_CIE2000.html}
#' @author Jon Clayden <code@@clayden.org>
#' @export
distance <- function (shades, reference)
{
    deg2rad <- function(x) x / 180 * pi
    rad2deg <- function(x) x / pi * 180
    
    shadeCoords <- coords(warp(shades, "Lab"))
    L2 <- shadeCoords[,1]
    a2 <- shadeCoords[,2]
    b2 <- shadeCoords[,3]
    
    if (length(reference) != 1L)
        stop("Reference should be a single shade")
    
    refCoords <- coords(warp(reference, "Lab"))
    L1 <- refCoords[,1]
    a1 <- refCoords[,2]
    b1 <- refCoords[,3]
    
    LBarPrime <- (L1 + L2) / 2
    C1 <- sqrt(a1^2 + b1^2)
    C2 <- sqrt(a2^2 + b2^2)
    CBar <- (C1 + C2) / 2
    G <- (1 - sqrt(CBar^7/(CBar^7+25^7))) / 2
    a1Prime <- a1 * (1+G)
    a2Prime <- a2 * (1+G)
    C1Prime <- sqrt(a1Prime^2 + b1^2)
    C2Prime <- sqrt(a2Prime^2 + b2^2)
    CBarPrime <- (C1Prime + C2Prime) / 2
    h1Prime <- atan2(b1, a1Prime) %% (2*pi)
    h2Prime <- atan2(b2, a2Prime) %% (2*pi)
    largeDiff <- abs(h1Prime-h2Prime) > pi
    HBarPrime <- (h1Prime + h2Prime + ifelse(largeDiff, 2*pi, 0)) / 2
    bigT <- 1 - 0.17*cos(HBarPrime-deg2rad(30)) + 0.24*cos(2*HBarPrime) + 0.32*cos(3*HBarPrime+deg2rad(6)) - 0.20*cos(4*HBarPrime-deg2rad(63))
    deltahPrime <- h2Prime - h1Prime + ifelse(largeDiff, ifelse(h2Prime>h1Prime,-2*pi,2*pi), 0)
    deltaLPrime <- L2 - L1
    deltaCPrime <- C2Prime - C1Prime
    deltaHPrime <- 2 * sqrt(C1Prime*C2Prime) * sin(deltahPrime/2)
    SL <- 1 + 0.015 * (LBarPrime-50)^2 / sqrt(20 + (LBarPrime-50)^2)
    SC <- 1 + 0.045 * CBarPrime
    SH <- 1 + 0.015 * CBarPrime * bigT
    deltatheta <- deg2rad(30 * exp(-((rad2deg(HBarPrime)-275) / 25)^2))
    RC <- 2 * sqrt(CBarPrime^7 / (CBarPrime^7 + 25^7))
    RT <- -RC * sin(2*deltatheta)
    
    unname(sqrt((deltaLPrime/SL)^2 + (deltaCPrime/SC)^2 + (deltaHPrime/SH)^2 + RT*(deltaCPrime/SC)*(deltaHPrime/SH)))
}
