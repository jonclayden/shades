context("Working with transparency")

test_that("opacity can be obtained and manipulated", {
    expect_equal(opacity("red"), 1)
    expect_equal(opacity("#FF000080"), 128/255)
    expect_equal(opacity("red",c(0,0.5)), c("#FF000000","#FF000080"), hexonly=TRUE)
    expect_equal(opacity("red",delta(-0.5)), "#FF000080", hexonly=TRUE)
    expect_null(attr(opacity("#FF000080",delta(0.5)),"alpha"))
    
    shades <- shade(c("red","green","blue"))
    expect_null(attr(shades,"alpha"))
    expect_equal(opacity(c(shades,"#00000000")), c(1,1,1,0))
    shades[2] <- "#00FF0000"
    expect_equal(attr(shades,"alpha"), c(1,0,1))
})
