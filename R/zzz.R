#' @import grDevices
#' @importFrom graphics par rect plot
.onLoad <- function (libname, pkgname)
{
    # Illuminant E
    E <- coords(warp(shade(.equalEnergy,space="XYZ"), "LMS"))
    
    abc <- sapply(.standardObserver, function(xyz) {
        A <- coords(warp(shade(xyz,space="XYZ"), "LMS"))
        a <- E[,2] * A[,3] - E[,3] * A[,2]
        b <- E[,3] * A[,1] - E[,1] * A[,3]
        c <- E[,1] * A[,2] - E[,2] * A[,1]
        c(a, b, c)
    })
    
    # Coefficients used by dichromat()
    .cache$a <- abc[1,]
    .cache$b <- abc[2,]
    .cache$c <- abc[3,]
    
    # Ratios of components of E, also used by dichromat()
    .cache$Er <- c(E[,3]/E[,2], E[,3]/E[,1], E[,2]/E[,1])
}
