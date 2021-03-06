#parameters
lambdas <- c(.001, .01, .1, 1)
epochs <- 50
steps <- 300
percent_training <- .8
percent_validation <- .1
percent_test <- .1
num_examples_epoch_test <- 50
steps_til_eval <- 30
steplength_a <- .01
steplength_b <- 50

#read files and create data set
raw_train_data <- read.csv('adult.data', header=FALSE, na.strings = "?")
raw_test_data <- read.csv('adult.test', header=FALSE, na.strings = "?")
raw_data <- rbind(raw_train_data, raw_test_data, make.row.names=FALSE)

#Split continuous and label data
#continuous variables: age, fnlwgt, education-num, capital-gain, capital-loss, hours-per-week
x_vector <- raw_data[,c(1,3,5,11,12,13)]
y_labels <- raw_data[,15]

#add scaling
for (i in 1:6){
  x_vector[i] <- scale(as.numeric(as.matrix(x_vector[i])))
}

#Mentions of positive and negative examples for reference
neg_example <- y_labels[1]
neg_example2 <- y_labels[48842]
pos_example <- y_labels[8]
pos_example2 <- y_labels[48843]

#split data into training, test, and validation sets
datasplit <- createDataPartition(y=y_labels, p=.8, list=FALSE)
trainx <- x_vector[datasplit,]
trainy <- y_labels[datasplit]
otherx <- x_vector[-datasplit,]
othery <- y_labels[-datasplit]
datasplit2 <- createDataPartition(y=othery, p=.5, list=FALSE)
testx <- otherx[datasplit2,]
testy <- othery[datasplit2]
valx <- otherx[-datasplit2,]
valy <- othery[-datasplit2]

#SVM Training
#loss function for SVM:
hinge_loss <- function(predicted, actual){
  return (max(0, 1 - (predicted * actual) ))
}

#evaluation for specific example x (6 items in vector) with parameters a and b
evaluate <- function(x, a, b){
  new_x <- as.numeric(as.matrix(x))
  return (t(a) %*% new_x + b) 
}

#Change y in dataset from <=50k and >50k to -1 and 1
converty <- function(y){
  if(y == neg_example | y == neg_example2){
    return (-1)
  }
  else if(y == pos_example | y == pos_example2){
    return (1)
  }
  else{
    return(NA)
  }
}

convertpred <- function(val){
  if(val >= 0){
    return(1)
  }
  else{
    return(-1)
  }
}

accuracy <- function(x,y,a,b){
  correct <- 0
  wrong <- 0
  for (i in 1:length(y)){
    pred <- evaluate(x[i,], a, b)
    pred <- convertpred(pred)
    actual <- converty(y[i])
    
    if(pred == actual){
      correct <- correct + 1 
    } else{
      wrong <- wrong + 1
    }
  }
  return(c( (correct/(correct+wrong)), correct, wrong) )
}

#array of validation accuracies
val_accuracies = c()
test_accuracies = c()

for (lambda in lambdas){
  #random initialization
  #a <- runif(dim(x_vector)[2], min=-.0001, max=.0001)
  #b <- runif(1, min=0, max=.01)
  a <- c(0,0,0,0,0,0)
  b <- 0
  
  accuracies <- c()
  posup <- 0
  negup <- 0
  
  for (epoch in 1:epochs){
    
    #set out 50 examples for testing after every 30 steps
    ran_vals <- sample(1:dim(trainx)[1], 50)
    accuracy_data <- trainx[ran_vals, ]
    accuracy_labels <- trainy[ran_vals]
    train_data <- trainx[-ran_vals,]
    train_labels <- trainy[-ran_vals]
    
    #Keep track of the number of steps taken at each epoch for debugging purposes
    num_steps <- 0
    
    for (step in 1:steps){
      
      if(num_steps %% steps_til_eval == 0){
        calc <- accuracy(accuracy_data, accuracy_labels, a, b)
        accuracies <- c(accuracies, calc[1]) 
        #print(calc[1])
      }
      
      k <- sample(1:length(train_labels), 1)
      while(is.na( converty( train_labels[k] ) )){
        k <- sample(1:length(train_labels), 1)
      }
      xex <- as.numeric(as.matrix( train_data[k,] ))
      yex <- converty( train_labels[k] )
      
      pred <- evaluate(xex, a, b)
      steplength = 1 / ((steplength_a * epoch) + steplength_b)
      
      
      #gradient vectors
      if(yex * pred >= 1){
        p1 <- lambda * a
        p2 <- 0
        posup <- posup + 1
      } else {
        p1 <- (lambda * a) - (yex * xex)
        p2 <- -(yex)
        negup <- negup + 1
      }
      
      #update values for a and b by gradient descent
      a <- a - (steplength * p1)
      b <- b - (steplength * p2)
      
      #update steps count
      num_steps <- num_steps + 1
    }
  }
  
  valeval <- accuracy (valx, valy, a, b)
  val_accuracies <- c(val_accuracies, valeval[1])
  
  testeval <- accuracy(testx, testy, a, b)
  test_accuracies <- c(test_accuracies, testeval[1])
  
  jpeg(file=paste(toString(lambda),".jpg") )
  title <- paste("Lambda = ", toString(lambda), " Accuracies Graph")
  plot(1:length(accuracies) , accuracies, type="o", col="blue", xlab ="Sample Time", ylab ="Accuracy", main = title)
  dev.off()
}

##Problem 2.5a
#A plot of the accuracy every 30 steps, for each value of the regularization constant
#plotted from within the SVM function

##Problem 2.5b
#Your estimate of the best value of the regularization constant, together with a brief description of why you believe that is a good value.
max_index <- 1
for(i in 1:length(val_accuracies)){
  if (val_accuracies[i] >= val_accuracies[max_index]){
    max_index <- i
  }
}
max_lambda <- lambdas[max_index]
max_lambda

##Problem 2.5c
#Your estimate of the accuracy of the best classifier on held out data
test_accuracies[max_index]