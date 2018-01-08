context("Graphics")

test_that("the swatch function works", {
    expect_null(swatch(c("red", "green", "blue")))
    grDevices::dev.off()
})
