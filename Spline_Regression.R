
# Regression Spline for Google Kaggle Competition

library(splines)

# Reading in the training and testing data
setwd('C:/Users/cathe/Desktop/SYS6018')
train <- read.csv('cleaned_train.csv')
# Adding a column for the log of the transaction revenues
train$log.transactionRevenue <- log(train$totals.transactionRevenue)
train[which(train$log.transactionRevenue <0),'log.transactionRevenue'] <- 0
test <- read.csv('cleaned_test.csv')


# Dividing training set into training and validation
set.seed(1996)
train.indices <- sample(1:length(train$channelGrouping), length(train$channelGrouping)/2)

train.cv <- train[train.indices, ]
# Adding a column for the log of the transaction revenues
train.cv$log.transactionRevenue <- log(train.cv$totals.transactionRevenue)
train.cv[which(train.cv$log.transactionRevenue <0),'log.transactionRevenue'] <- 0

valid.cv <- train[-train.indices, ]
# Adding a column for the log of the transaction revenues
valid.cv$log.transactionRevenue <- log(valid.cv$totals.transactionRevenue)
valid.cv[which(valid.cv$log.transactionRevenue <0),'log.transactionRevenue'] <- 0

# Fitting the Regresion Splines
sig.col.indices <- c(36, 6, 40, 37, 19, 5, 2) # received this list from our random forest
sig.cols <- colnames(train)[sig.col.indices]


# Testing different number of knots by comparing MSE
mses <- c()
for(i in 1:15){
  model.rs <- lm(log.transactionRevenue ~ bs(Month, knots = i) + bs(visitStartTime, knots = i) + 
                   bs(Dayofyear, knots = i) + bs(Week, knots = i) + bs(totals.hits, knots = i), data = train.cv)
  preds.rs <- predict(model.rs, newdata = valid.cv)
  mses[i] <- mean((valid.cv[,'log.transactionRevenue'] - preds.rs)^2)
}
plot(x = 1:15, y = mses)
# The elbow of the functin of MSE vs number of knots shows 12 knots at the elbow, 
# so 12 knots should be used for each regressor.

# Fitting to the full training set and making predictions
model.rs <- lm(log.transactionRevenue ~ bs(Month, knots = 12) + bs(visitStartTime, knots = 12) + 
                 bs(Dayofyear, knots = 12) + bs(Week, knots = 12) + bs(totals.hits, knots = 12), data = train)
preds.rs <- predict(model.rs, newdata = test)

# Writing Predictions to a csv file
final.ansr <- cbind(test$fullVisitorId, preds.rs)
write.table(final.ansr, file="regression_spline1.csv", row.names=F, col.names = c("fullVisitorId", "PredictedLogRevenue"), sep=',')




