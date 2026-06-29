# Create shade objects from various objects and none
expect_length(shade(), 0L)
expect_equal(shade(), shade(character(0)))
expect_equal(names(shade("red")), "red")
expect_stdout(print(shade("red")), "1 named shade")
expect_equal(space(shade("red")), "sRGB")
expect_equivalent(coords("red"), matrix(c(1,0,0),nrow=1))
expect_equivalent(shade(matrix(c(1,0,0),nrow=1)), shade("#FF0000"))
# Using a factor to check the default initialisation method
expect_equivalent(shade(factor(rep("red",3))), rep(shade("red"),3))

shade <- shade("red")
shade[1] <- "#0bead5"
expect_null(names(shade))

# Try the shades() interface
randomRgbCoords <- matrix(runif(6), nrow=2)
expect_equal(shades(red="#ee0000", random=randomRgbCoords),
             c(shade(c(red="#ee0000")), structure(shade(randomRgbCoords), names=rep("random",2))))

if (requireNamespace("colorspace", quietly=TRUE))
    expect_true(shade(colorspace::sRGB(1,0,0)) == "red")

# Convert shade objects between spaces
expect_equivalent(coords(warp("red","HSV")), matrix(c(0,1,1),nrow=1))
expect_equivalent(round(coords(warp(shade("red"),"LAB"))), matrix(c(53,80,67),nrow=1))
# Check that precision loss isn't too great when making a round-trip conversion
shade <- shade(matrix(runif(3),nrow=1), space="sRGB")
expect_equivalent(coords(warp(warp(shade,"HSV"),"sRGB")), coords(shade), tolerance=1e-4)

# Index, combine and compare shade objects
shades <- shade(c("red","green","blue"))
expect_true(shades[1] == "red")
shades[1] <- "darkred"
expect_equal(names(shades)[1], "darkred")
expect_false(shades[1] == "red")

expect_equal(c(shade("red"),shade("green")), shade(c("red","green")))
expect_true(shade("red") != shade("green"))
expect_true(shade("red") == shade(matrix(c(0,1,1),nrow=1),space="HSV"))
expect_equal(space(c(shade("red"), shade(matrix(c(0,1,1),nrow=1),space="HSV"))), "XYZ")

expect_match(all.equal(shade("red"),shade("green")), "Mean colour distance is")
expect_match(all.equal(shade("red"),shade(c("green","blue"))), "Lengths do not match")
