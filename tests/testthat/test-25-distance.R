context("Calculating colour distances")

test_that("colour distances can be calculated", {
    expect_equal(round(distance(c("red","green","blue"),"red")), c(0,87,53))
})