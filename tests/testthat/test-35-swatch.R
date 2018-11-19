context("Graphics")

test_that("the swatch function works", {
    expect_null(swatch(c("red", "green", "blue")))
    grDevices::dev.off()
    
    expect_null(swatch(saturation(brightness(c("red", "green", "blue"), c(0.25,0.75)), c(0.25,0.75))))
    grDevices::dev.off()
})
