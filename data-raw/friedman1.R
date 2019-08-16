# Script used to generate the friedman1 dataset using the tgp library.

library(tgp)

set.seed(1234)

friedman1 <- friedman.1.data(n = 1000)

friedman1 <- friedman1[,-11]

friedman1 <- scale_zero_one(friedman1)

usethis::use_data(friedman1, overwrite = TRUE)

