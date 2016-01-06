context("Creating and converting shade objects")

test_that("shade objects can be created from various objects", {
    expect_equal(attr(shade("red"),"space"), "sRGB")
    expect_equivalent(attr(shade("red"),"coords"), matrix(c(1,0,0),nrow=1))
    expect_equivalent(shade(colorspace::sRGB(1,0,0)), shade("#FF0000"))
    expect_equivalent(shade(matrix(c(1,0,0),nrow=1)), shade("#FF0000"))
})

test_that("shade objects can be converted between spaces", {
    expect_equal(as(shade("red"),"HSV"), colorspace::HSV(360,1,1))
    expect_equivalent(round(colorspace::coords(as(shade("red"),"LAB"))), matrix(c(53,80,67),nrow=1))
})
