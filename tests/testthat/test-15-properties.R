context("Extracting and modifying colour properties")

test_that("colour properties can be extracted", {
    expect_equal(saturation("red"), 1)
    expect_equal(brightness("red"), 1)
    expect_equal(saturation("grey40"), 0)
    expect_equal(brightness(c("grey40","grey60")), c(0.4,0.6))
})

test_that("colour properties can be manipulated", {
    expect_equivalent(saturation("red",0.5), shade("#FF8080"))
    expect_equivalent(brightness("red",0.5), shade("#800000"))
    expect_equivalent(hueshift("red",240), shade("#0000FF"))
    expect_equal(attr(brightness("grey40",c(0.2,0.6)),"coords"), colorspace::coords(as(shade(c("grey20","grey60")),"HSV")))
})
