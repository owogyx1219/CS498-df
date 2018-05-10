library(InformationValue)
library(glmnet)

rawData <- read.csv(file="credit-card-data.csv", header=TRUE, sep=",")
cols <- ncol(rawData)

rawData <- rawData[, 2:cols]
for(i in 2:4)
{
  rawData[, i] <- factor(rawData[, i])
}

for(i in 6:11)
{
  rawData[, i] <- factor(rawData[, i])
}

#rawData[, 24] <- factor(rawData[, 24])

model <- glm(default.payment.next.month ~.,data=rawData, family = "binomial")
#summary(model)
print(min(model$cvm))

predicted <- predict(model, rawData[, 1:24], type="response")

error <- misClassError(rawData$default.payment.next.month, predicted, threshold = 0.5)
print(error)

table(rawData$default.payment.next.month, predicted > 0.5)

print(ncol(rawData))
default <- rawData[, 24]

#model_with_ridge <- cv.glmnet(x=data.matrix(rawData[, 1:23]),y=default, alpha = 0)
#accuracy_ridge <- min(model_with_ridge$cvm)

#fitted_ridge <- predict(model_with_ridge, data.matrix(rawData[, 1:23]))
#table(fitted_ridge, default)
#fitted_ridge <- ifelse(fitted_ridge > 0.3, 1, 0)
#print(default[1:50])
#print(fitted_ridge[1:50])
model_with_ridge <- cv.glmnet(x=data.matrix(rawData[, 1:23]), y=default, family="binomial", nfolds=10, alpha=0)

fitted_ridge <- predict(model_with_ridge, data.matrix(rawData[, 1:23]))
#print(fitted_ridge[1:50])

print(min(model_with_ridge$cvm))
plot(model_with_ridge)
#print(accuracy_ridge)

#accuracy_ridge <- min(model_with_ridge$cvm)

#summary(model_with_ridge)
#print(accuracy_ridge)

model_with_elastic_net1 <- cv.glmnet(x=data.matrix(rawData[, 1:23]), y=default, family="binomial", nfolds=10, alpha=0.25)
print(min(model_with_elastic_net1$cvm))
plot(model_with_elastic_net1)

model_with_elastic_net2 <- cv.glmnet(x=data.matrix(rawData[, 1:23]), y=default, family="binomial", nfolds=10, alpha=0.5)
print(min(model_with_elastic_net2$cvm))
plot(model_with_elastic_net2)

model_with_elastic_net3 <- cv.glmnet(x=data.matrix(rawData[, 1:23]), y=default, family="binomial", nfolds=10, alpha=0.75)
print(min(model_with_elastic_net3$cvm))
plot(model_with_elastic_net3)

model_with_lasso <- cv.glmnet(x=data.matrix(rawData[, 1:23]), y=default, family="binomial", nfolds=10, alpha=1)
print(min(model_with_lasso$cvm))
plot(model_with_lasso)


