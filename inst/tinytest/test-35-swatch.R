expect_null(swatch(c("red", "green", "blue")))
expect_null(swatch(saturation(brightness(c("red", "green", "blue"), c(0.25,0.75)), c(0.25,0.75))))
