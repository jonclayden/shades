using("shades")

# Create colour scales traversing different spaces
expect_equal_shades(gradient(c("red","blue"),3), c("#FF0000","#800080","#0000FF"))
expect_equal_shades(rev(gradient(c("red","blue"),3)), c("#0000FF","#800080","#FF0000"))
expect_equal_shades(gradient(c("red","blue"),3,space="LAB"), c("#FF0000","#C90089","#0000FF"))
expect_equal_shades(gradient("magma",3), c("#000004","#B63779","#FCFDBF"))
expect_error(gradient("nothing",3), "should specify a predefined colour map")
