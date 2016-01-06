context("Creating colour scales")

test_that("colour scales can traverse different spaces", {
    expect_equivalent(gradient(c("red","blue"),3), shade(c("#FF0000","#800080","#0000FF")))
    expect_equivalent(gradient(c("red","blue"),3,space="LAB"), shade(c("#FF0000","#CA0088","#0000FF")))
})
