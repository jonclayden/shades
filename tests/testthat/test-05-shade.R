context("Creating and converting shade objects")

test_that("shade objects can be created from various objects", {
    expect_error(shade(), "must not be empty")
    expect_error(shade(character(0)), "must not be empty")
    expect_output(print(shade("red")), "1 shade")
    expect_equal(space(shade("red")), "sRGB")
    expect_equivalent(coords("red"), matrix(c(1,0,0),nrow=1))
    expect_equivalent(shade(matrix(c(1,0,0),nrow=1)), shade("#FF0000"))
    # Using a factor to check the default initialisation method
    expect_equivalent(shade(factor(rep("red",3))), rep(shade("red"),3))
    
    if (system.file(package="colorspace") == "")
        skip("The \"colorspace\" package is not available")
    else
        expect_true(shade(colorspace::sRGB(1,0,0)) == "red")
})

test_that("shade objects can be converted between spaces", {
    expect_equivalent(coords(warp("red","HSV")), matrix(c(0,1,1),nrow=1))
    expect_equivalent(round(coords(warp(shade("red"),"LAB"))), matrix(c(53,80,67),nrow=1))
})

test_that("shade object can be indexed, combined and compared", {
    shades <- shade(c("red","green","blue"))
    expect_true(shades[1] == "red")
    shades[1] <- "darkred"
    expect_false(shades[1] == "red")
    
    expect_equal(c(shade("red"),shade("green")), shade(c("red","green")))
    expect_true(shade("red") != shade("green"))
    expect_true(shade("red") == shade(matrix(c(0,1,1),nrow=1),space="HSV"))
    expect_equal(space(c(shade("red"), shade(matrix(c(0,1,1),nrow=1),space="HSV"))), "XYZ")
    
    expect_match(all.equal(shade("red"),shade("green")), "Mean colour distance is")
})
