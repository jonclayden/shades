context("Extracting and modifying colour properties")

test_that("colour properties can be extracted", {
    expect_equal(saturation("red"), 1)
    expect_equal(brightness("red"), 1)
    expect_equal(saturation("grey40"), 0)
    expect_equal(brightness(c("grey40","grey60")), c(0.4,0.6))
})

test_that("colour properties can be manipulated", {
    expect_equal(saturation("red",0.5), shade("#FF8080"), hexonly=TRUE)
    expect_equal(brightness("red",0.5), shade("#800000"), hexonly=TRUE)
    expect_equal(hue("red",delta(240)), shade("#0000FF"), hexonly=TRUE)
    expect_equal(hue("blue",delta(240)), shade("#00FF00"), hexonly=TRUE)
    expect_equivalent(coords(brightness("grey40",c(0.2,0.6))), matrix(c(0,0,0,0,0.2,0.6),nrow=2))
})
