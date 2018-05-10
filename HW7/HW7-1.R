numOfDocs = 1500
numOfWords = 12419
numOfTopics = 30

raw_data <- read.csv("docword.nips.txt", sep = " ", header = FALSE, skip = 3)
raw_vocab <- unlist(read.csv("vocab.nips.txt", stringsAsFactors = FALSE))


length <- dim(raw_data)[1]
mat <- matrix(0, nrow = numOfDocs, ncol = numOfWords)
for(i in 1:length)
{
  rowIndex <- raw_data[i, 1]
  colIndex <- raw_data[i, 2]
  mat[rowIndex, colIndex] <- raw_data[i, 3]
}

P_i_j <- matrix(1/numOfTopics, nrow = 1, ncol = numOfTopics)
P_j_k <- matrix(0, nrow = numOfTopics, ncol = numOfWords)
for(j in 1:numOfTopics)
{
  uniDistribution <- runif(numOfWords)
  P_j_k[j, ] <- uniDistribution / sum(uniDistribution)
}


Qs <- c()
P_i_js <- c()
for(i in 1:100)
{
  #E Step
  product <- mat %*% t(log(P_j_k)) 
  weight <- matrix(0, nrow = numOfDocs, ncol = numOfTopics)
  for(i in 1:numOfTopics)
  {
    weight[, i] <- product[, i] + log(P_i_j[i])
  }
  
  weight_copy <- weight
  maxInRow <- apply(weight, 1, max)
  temp <- matrix(0, numOfDocs)
  for(i in 1:numOfDocs)
  {
    temp[i] <- log(sum(exp(weight_copy[i, ] - maxInRow[i])))
  }
  
  W_i_j <- matrix(0, nrow = numOfDocs, ncol = numOfTopics)
  W <- exp(weight_copy - unlist(as.list(maxInRow - temp)))
  for(i in 1:numOfDocs)
  {
    W_i_j[i, ] <- W[i, ] / sum(W[i, ])
  }
  
  Q <- sum(weight*W)
  Qs <- c(Qs, Q)
  
  #M Step
  c <- 0.00025
  for(i in 1:numOfTopics)
  {
    colAtI <- W_i_j[, i]
    P_j_k[i, ] <- colSums(mat*colAtI + c) / ( sum(rowSums(mat)*colAtI) + (c*numOfWords) ) 
    P_i_j[i] <- sum(W_i_j[, i] / numOfDocs)
  }
  
  #print(P_i_j)
  P_i_js <- c(P_i_js, P_i_j)
  diff <- Q - Qs[length(Qs)-1]
  diff2 <- sum(P_i_j) - sum(P_i_js[length(P_i_js) - 1])
  #print(diff2)
  if(length(Qs) > 1 && diff2 < 0.988)
  {
    break
  }
}


#plot the probility that a topic is selected
plot(unlist(as.list(P_i_j)), type='l', xlab="Topic", ylab = "Probability", main = "Probability That A Topic Is Selected")


#plot the a table showing, for each topic, the 10 words with the highest probability for that topic.
vocab_mat <- c()
row_names <- c()
for(i in 1:numOfTopics)
{
  sortedP <- sort(P_j_k[i, ], decreasing = TRUE)
  threshold <- sortedP[10]
  tenTopics <- raw_vocab[which(P_j_k[i, ] >= threshold)][1:10]
  vocab_mat <- c(vocab_mat, tenTopics)
  name <- paste("Topic", i, ":")
  row_names <- c(row_names, name)
  #writeLines(paste("Topic", i, ":", tenTopics), output.txt)
  #cat(line, file="output.txt", sep="\n", append=TRUE)
}

vocab_mat <- matrix(vocab_mat, nrow = 30)
row.names(vocab_mat) <- row_names
write.csv(vocab_mat, file = "output.csv", row.names = TRUE, col.names = FALSE)
