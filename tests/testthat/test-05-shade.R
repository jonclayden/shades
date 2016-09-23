context("Creating and converting shade objects")

test_that("shade objects can be created from various objects", {
    expect_equal(space(shade("red")), "sRGB")
    expect_equivalent(coords(shade("red")), matrix(c(1,0,0),nrow=1))
    expect_equivalent(shade(matrix(c(1,0,0),nrow=1)), shade("#FF0000"))
    expect_true(shade(c("red","green","blue"))[1] == "red")
    
    if (system.file(package="colorspace") == "")
        skip("The \"colorspace\" package is not available")
    else
        expect_true(shade(colorspace::sRGB(1,0,0)) == "red")
})

test_that("shade objects can be converted between spaces", {
    expect_equivalent(coords(warp("red","HSV")), matrix(c(0,1,1),nrow=1))
    expect_equivalent(round(coords(warp(shade("red"),"LAB"))), matrix(c(53,80,67),nrow=1))
})
