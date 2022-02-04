# This code is only for comparison with our implemented method
library(alr4)
x_cols = c('Ht','Wt','LBM','BMI','SSF')

X = scale(as.matrix(ais[,x_cols]))
Y = scale(as.vector(ais[,"Bfat"]))

coefs <-lm(Y~X-1)

coefs
