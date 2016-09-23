context("Mixing colours")

test_that("shade objects can be mixed and complements calculated", {
    expect_equivalent(coords(complement("cyan")), coords(shade("red")))
    expect_equal(complement("cyan",space="HSV"), warp("red","HSV"))
    expect_equal(complement("cyan",space="Lab"), "#4F0002", hexonly=TRUE)
    
    expect_equivalent(addmix("blue","green"), shade("#00FFFF"))
    expect_equivalent(submix("cyan","yellow"), shade("#00FF00"))
})