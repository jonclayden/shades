context("Creating colour scales")

test_that("colour scales can traverse different spaces", {
    expect_equal(gradient(c("red","blue"),3), c("#FF0000","#800080","#0000FF"), hexonly=TRUE)
    expect_equal(rev(gradient(c("red","blue"),3)), c("#0000FF","#800080","#FF0000"), hexonly=TRUE)
    expect_equal(gradient(c("red","blue"),3,space="LAB"), c("#FF0000","#C90089","#0000FF"), hexonly=TRUE)
    expect_equal(gradient("magma",3), c("#000004","#B63779","#FCFDBF"), hexonly=TRUE)
    expect_error(gradient("nothing",3), "should specify a predefined colour map")
})
