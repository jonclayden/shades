context("Extracting and modifying colour properties")

test_that("colour properties can be extracted", {
    expect_equal(saturation("red"), 1)
    expect_equal(brightness("red"), 1)
    expect_equal(saturation("grey40"), 0)
    expect_equal(brightness(c("grey40","grey60")), c(0.4,0.6))
    expect_equal(round(chroma(c("black","white","red"))), c(0,0,105))
    expect_equal(round(lightness(c("black","white","red"))), c(0,100,53))
})

test_that("colour properties can be manipulated", {
    expect_equal(saturation("red",0.5), shade("#FF8080"), hexonly=TRUE)
    expect_equal(brightness("red",0.5), shade("#800000"), hexonly=TRUE)
    expect_equal(hue("red",delta(240)), shade("#0000FF"), hexonly=TRUE)
    expect_equal(hue("blue",delta(240)), shade("#00FF00"), hexonly=TRUE)
    expect_equal(lightness("red",scalefac(0.5)), shade("#A60000"), hexonly=TRUE)
    expect_equal(brightness("red",scalefac(0.3,0.5)), c("#4D0000","#800000"), hexonly=TRUE)
    expect_equivalent(coords(brightness("grey40",c(0.2,0.6))), matrix(c(0,0,0,0,0.2,0.6),nrow=2))
})

test_that("manipulated colour matrices have the expected dimensions", {
    shades <- shade(c("red","green","blue"))
    matrix <- rep(shades, 2)
    dim(matrix) <- c(3L,2L)
    
    expect_null(dim(brightness(shades,0.5)))
    expect_equal(length(brightness(shades,0.5)), 3L)
    expect_equal(dim(brightness(shades,c(0.4,0.6))), c(2L,3L))
    expect_equal(brightness(shades,c(0.4,0.6)), c("#660000","#990000","#006600","#009900","#000066","#000099"), hexonly=TRUE)
    expect_equal(dim(brightness(matrix,c(0.4,0.6))), c(2L,3L,2L))
    expect_equal(dim(brightness(matrix,recycle(0.4,0.6))), c(3L,2L))
})
