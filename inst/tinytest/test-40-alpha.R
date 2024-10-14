using("shades")

expect_equal(opacity("red"), 1)
expect_equal(opacity("#FF000080"), 128/255)
expect_equal(opacity(saturation("#FF000080",0.5)), 128/255)
expect_equal_shades(warp("#FF000080","HSV"), "#FF000080")

expect_equal_shades(opacity("red",c(0,0.5)), c("#FF000000","#FF000080"))
expect_equal_shades(opacity("red",delta(-0.5)), "#FF000080")
expect_null(attr(opacity("#FF000080",delta(0.5)),"alpha"))

shades <- shade(c("red","green","blue"))
matrix <- rep(shades, 2)
dim(matrix) <- c(3L,2L)

expect_null(attr(shades,"alpha"))
expect_equal(opacity(c(shades,"#00000000")), c(1,1,1,0))
shades[2] <- "#00FF0000"
expect_equal(attr(shades,"alpha"), c(1,0,1))
expect_equal(dim(opacity(matrix,0.5)), c(3L,2L))
expect_equal(dim(opacity(matrix,recycle(0.4,0.6))), c(3L,2L))

expect_true(shade("#FF8080") == shade("#FF8080FF"))
expect_false(shade("#FF8080") == shade("#FF8080DD"))
expect_true(grepl("Alpha values do not match", all.equal(shade("#FF8080"), "#FF8080DD")))
