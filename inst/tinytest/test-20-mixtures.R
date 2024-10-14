using("shades")

# Colour mixing and complements
expect_equivalent(coords(complement("cyan")), coords(shade("red")))
expect_equal(complement("cyan",space="HSV"), warp("red","HSV"))
expect_equal_shades(complement("cyan",space="Lab"), "#4F0002")

expect_equivalent(addmix("blue","green"), shade("#00FFFF"))
expect_equivalent(submix("cyan","yellow"), shade("#00FF00"))
expect_equivalent("blue" %.)% "green", shade("#00FFFF"))
expect_equivalent("cyan" %_/% "yellow", shade("#00FF00"))
