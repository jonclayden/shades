using("shades")

primaries <- shade(c("red", "green", "blue"))
expect_equal_shades(dichromat(primaries), c("#4D4222","#FFF600","#0027FF"))
expect_equal_shades(dichromat(primaries,"protanopic"), c("#4D4222","#FFF600","#0027FF"))
expect_equal_shades(dichromat(primaries,"deuteranopic"), c("#C5A700","#D0B335","#0068FE"))
expect_equal_shades(dichromat(primaries,"tritanopic"), c("#FF0050","#00FBFF","#372828"))
