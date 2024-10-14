using("shades")

# Missing shade handling
shades <- gradient(c("red","blue"), 3)
shades[2] <- NA

expect_equal(is.na(shades), c(FALSE,TRUE,FALSE))
expect_true(all(is.na(coords(shades)[2,])))
expect_equal(saturation(shades), c(1,NA,1))
expect_equal_shades(saturation(shades,0.5), shade(c("#FF8080",NA,"#8080FF")))
expect_equivalent(is.na(saturation(shades, c(0,0.5,1))), matrix(c(FALSE,TRUE,FALSE),3,3,byrow=TRUE))
expect_equivalent(complement(shades), shade(c("#00FFFF",NA,"#FFFF00")))
expect_equivalent(dichromat(shades)[2], shade(NA))
expect_equal_shades(opacity(shades,0.5), c("#FF000080",NA,"#0000FF80"))

# NAs as new property values should lead to pass-through
expect_equal(saturation(saturation("olivedrab",NA)), saturation("olivedrab"))
expect_equal_shades(opacity("red",NA), "red")
expect_equal_shades(opacity("red",c(0,NA,1)), c("#FF000000","#FF0000FF","#FF0000FF"))
