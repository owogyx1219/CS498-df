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
print(min(model$cvm))
predicted <- predict(model, rawData[, 1:24], type="response")

confMatrix <- table(rawData$default.payment.next.month, predicted > 0.5)
colnames(confMatrix)[colnames(confMatrix) == 'FALSE'] <- '0'
colnames(confMatrix)[colnames(confMatrix) == 'TRUE'] <- '1'

print(confMatrix)
diagSum <- confMatrix[1,1] + confMatrix[2,2]
sum <- diagSum + confMatrix[1,2] + confMatrix[2,1]
accy <- diagSum / sum
print(accy)

default <- rawData[, 24]

calcAccuracy <- function(fittedValues, trueValues)
{
  right <- 0
  wrong <- 0
  for(i in 1:length(trueValues))
  {
    if(trueValues[i] == fittedValues[i])
    {
      right <- right + 1
    }
    else
    {
      wrong <- wrong + 1
    }
  }
  accuracy <- right / (right + wrong)
  
  return (accuracy)
}

buildModelAndGetAccuracy <- function(trainData, trainLabels,testData, testLabels, alphaValue)
{
  model <- cv.glmnet(x=data.matrix(trainData),y=trainLabels, family = "binomial", type.measure="class", nfolds=10, alpha = alphaValue)
  plot(model)
  #fitted_results <- predict(model, data.matrix(testData))
  #fitted_results <- ifelse(fitted_results > 0.4, 1, 0)
  #accy <- calcAccuracy(fitted_results, testLabels)
  errorRate <- min(model$cvm)
  accyRate <- 1- errorRate
  return (accyRate)
}

accy_ridge <- buildModelAndGetAccuracy(rawData[, 1:23], default, rawData[, 1:23], default, 0)
print(accy_ridge)

accy_elastic_net1 <- buildModelAndGetAccuracy(rawData[, 1:23], default, rawData[, 1:23], default, 0.25)
print(accy_elastic_net1)

accy_elastic_net2 <- buildModelAndGetAccuracy(rawData[, 1:23], default, rawData[, 1:23], default, 0.5)
print(accy_elastic_net2)

accy_elastic_net3 <- buildModelAndGetAccuracy(rawData[, 1:23], default, rawData[, 1:23], default, 0.75)
print(accy_elastic_net3)

accy_lasso <- buildModelAndGetAccuracy(rawData[, 1:23], default, rawData[, 1:23], default, 1)
print(accy_lasso)


