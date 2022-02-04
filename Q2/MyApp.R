# ____________________________ Q3 ____________________________

# library to include
library(alr4)
library(caret)
library(leaps)

# Data information
data(ais)
?ais

# 'Sex','Ht','Wt','LBM','RCC','WCC','Hc','Ferr','BMI','SSF'

# _______________________ a _______________________
# Full model
mlr = lm(Bfat ~ ., data = ais) 

# dimension of the data matrix
x<-dim(ais)

# number of rows
x[1]

# Stepwise function with BIC criterion in both directions 
model.both.bic = step(mlr, direction = "both", trace = FALSE, k = log(x[1]))
print(model.both.bic) # reported model
# REPORTED MODEL: (formula = Bfat ~ Wt + LBM + SSF + Label, data = ais)
# _______________________ b _______________________

# Cross-Validation specification 
ctrl <- trainControl(method = "cv", number = 7)

# Fitting a regression model and use 7-fold CV to evaluate performance [Full model]
#   RMSE       Rsquared  MAE     
# 0.7228159  0.986673  0.532802
fit.cv.full <- train(Bfat ~ ., data = ais, method = "lm", trControl = ctrl)
print(fit.cv.full) # Report

# Fitting a regression model and use 7-fold CV to evaluate performance [Stepwise BIC]
#   RMSE       Rsquared  MAE      
# 0.6920994  0.98758   0.5225645
fit.cv.stepbic <- train(Bfat ~ Wt + LBM + SSF + Label, data = ais, method = "lm", trControl = ctrl)
print(fit.cv.stepbic) # Report            

# _______________________ c _______________________

# Subset selection for the full model 
best1 <- regsubsets(Bfat ~ ., data = ais)
summary(best1)
result1<-summary(best1)
which.min(result1$cp)

# Subset selection for the stepwise model 
best2 <- regsubsets(Bfat ~ Wt + LBM + SSF + Label, data = ais)
summary(best2)
result2<-summary(best2)
which.min(result2$cp)

# both of models based on CP criteria says the model can be defined with 8 variables.