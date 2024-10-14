expect_equal(round(distance(c("red","green","blue"),"red")), c(0,87,53))
expect_error(distance("red", c("red","green","blue")), "Reference should be a single shade")
