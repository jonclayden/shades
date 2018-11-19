context("Colour adaptation")

test_that("dichromacy can be simulated", {
    primaries <- shade(c("red", "green", "blue"))
    expect_equivalent(dichromat(primaries), c("#4D4222","#FFF600","#0027FF"), hexonly=TRUE)
    expect_equivalent(dichromat(primaries,"protanopic"), c("#4D4222","#FFF600","#0027FF"), hexonly=TRUE)
    expect_equivalent(dichromat(primaries,"deuteranopic"), c("#C5A700","#D0B335","#0068FE"), hexonly=TRUE)
    expect_equivalent(dichromat(primaries,"tritanopic"), c("#FF0050","#00FBFF","#372828"), hexonly=TRUE)
})
