library(factoextra)
labels <- read.table("cifar-10-batches-bin/batches.meta.txt")
images.rgb <- list()
images.lab <- list()
train_index <- 0
num.images = 10000 # Set to 10000 to retrieve all images per file to memory
for (f in 1:5) {
  to.read <- file(paste("cifar-10-batches-bin/data_batch_", f, ".bin", sep=""), "rb")
  for(i in 1:num.images) {
    l <- readBin(to.read, integer(), size=1, n=1, endian="big")
    r <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    g <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    b <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    index <- num.images * (f-1) + i
    images.rgb[[index]] = data.frame(r, g, b)
    images.lab[[index]] = l+1
    train_index <- index
  }
  to.read <- file("cifar-10-batches-bin/test_batch.bin","rb")
  for(i in 1:num.images) {
    l <- readBin(to.read, integer(), size=1, n=1, endian="big")
    r <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    g <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    b <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    index <- train_index + i
    images.rgb[[index]] = data.frame(r, g, b)
    images.lab[[index]] = l+1
  }
  close(to.read)
}
remove(l,r,g,b,f,i,index, to.read)

images.rgb.1 <- list()
images.rgb.2 <- list()
images.rgb.3 <- list()
images.rgb.4 <- list()
images.rgb.5 <- list()
images.rgb.6 <- list()
images.rgb.7 <- list()
images.rgb.8 <- list()
images.rgb.9 <- list()
images.rgb.10 <- list()
num_1 = 1
num_2 = 1
num_3 = 1
num_4 = 1
num_5 = 1
num_6 = 1
num_7 = 1
num_8 = 1
num_9 = 1
num_10 = 1
for(index in 1:length(images.rgb)){
  if(images.lab[index] == 1){
    images.rgb.1[[num_1]] <- images.rgb[[index]]
    num_1 = num_1 + 1
  }
  else if(images.lab[index] == 2){
    images.rgb.2[[num_2]] <- images.rgb[[index]]
    num_2 = num_2 + 1
  }
  else if(images.lab[index] == 3){
    images.rgb.3[[num_3]] <- images.rgb[[index]]
    num_3 = num_3 + 1
  }
  else if(images.lab[index] == 4){
    images.rgb.4[[num_4]] <- images.rgb[[index]]
    num_4 = num_4 + 1
  }
  else if(images.lab[index] == 5){
    images.rgb.5[[num_5]] <- images.rgb[[index]]
    num_5 = num_5 + 1
  }
  else if(images.lab[index] == 6){
    images.rgb.6[[num_6]] <- images.rgb[[index]]
    num_6 = num_6 + 1
  }
  else if(images.lab[index] == 7){
    images.rgb.7[[num_7]] <- images.rgb[[index]]
    num_7 = num_7 + 1
  }
  else if(images.lab[index] == 8){
    images.rgb.8[[num_8]] <- images.rgb[[index]]
    num_8 = num_8 + 1
  }
  else if(images.lab[index] == 9){
    images.rgb.9[[num_9]] <- images.rgb[[index]]
    num_9 = num_9 + 1
  }
  else{
    images.rgb.10[[num_10]] <- images.rgb[[index]]
    num_10 = num_10 + 1
  }
}
remove(num_1,num_2,num_3,num_4,num_5,num_6,num_7,num_8,num_9,num_10)

calculateError <- function(eigValue){
  error <- 0.0
  for (i in 1:length(eigValue))
  {
    error <- error + abs(eigValue[i])
  }
  return (error)
}


value_1 = prcomp(as.data.frame(images.rgb.1),scale=FALSE, rank. = 20)
value_2 = prcomp(as.data.frame(images.rgb.2),scale=FALSE, rank. = 20)
value_3 = prcomp(as.data.frame(images.rgb.3),scale=FALSE, rank. = 20)
value_4 = prcomp(as.data.frame(images.rgb.4),scale=FALSE, rank. = 20)
value_5 = prcomp(as.data.frame(images.rgb.5),scale=FALSE, rank. = 20)
value_6 = prcomp(as.data.frame(images.rgb.6),scale=FALSE, rank. = 20)
value_7 = prcomp(as.data.frame(images.rgb.7),scale=FALSE, rank. = 20)
value_8 = prcomp(as.data.frame(images.rgb.8),scale=FALSE, rank. = 20)
value_9 = prcomp(as.data.frame(images.rgb.9),scale=FALSE, rank. = 20)
value_10 = prcomp(as.data.frame(images.rgb.10),scale=FALSE, rank. = 20)

eig.val1 <- get_eigenvalue(value_1)
eig.val2 <- get_eigenvalue(value_2)
eig.val3 <- get_eigenvalue(value_3)
eig.val4 <- get_eigenvalue(value_4)
eig.val5 <- get_eigenvalue(value_5)
eig.val6 <- get_eigenvalue(value_6)
eig.val7 <- get_eigenvalue(value_7)
eig.val8 <- get_eigenvalue(value_8)
eig.val9 <- get_eigenvalue(value_9)
eig.val10 <- get_eigenvalue(value_10)

pComponent1 <- eig.val1[1:20,1]
pComponent2 <- eig.val2[1:20,1]
pComponent3 <- eig.val3[1:20,1]
pComponent4 <- eig.val4[1:20,1]
pComponent5 <- eig.val5[1:20,1]
pComponent6 <- eig.val6[1:20,1]
pComponent7 <- eig.val7[1:20,1]
pComponent8 <- eig.val8[1:20,1]
pComponent9 <- eig.val9[1:20,1]
pComponent10 <- eig.val10[1:20,1]

restComponent1 <- eig.val1[21:1024,1]
restComponent2 <- eig.val2[21:1024,1]
restComponent3 <- eig.val3[21:1024,1]
restComponent4 <- eig.val4[21:1024,1]
restComponent5 <- eig.val5[21:1024,1]
restComponent6 <- eig.val6[21:1024,1]
restComponent7 <- eig.val7[21:1024,1]
restComponent8 <- eig.val8[21:1024,1]
restComponent9 <- eig.val9[21:1024,1]
restComponent10 <- eig.val10[21:1024,1]


error1 <- calculateError(restComponent1)
error2 <- calculateError(restComponent2)
error3 <- calculateError(restComponent3)
error4 <- calculateError(restComponent4)
error5 <- calculateError(restComponent5)
error6 <- calculateError(restComponent6)
error7 <- calculateError(restComponent7)
error8 <- calculateError(restComponent8)
error9 <- calculateError(restComponent9)
error10 <- calculateError(restComponent10)


H <- c(error1, error2, error3, error4, error5, error6, error7, error8, error9, error10)
png(file = "barchart.png")
barplot(H, width=200, names.arg=c("air", "auto", "bird", "cat", "deer", "dog", "frog", "horse", "ship", "truck"), main="Bar Chart of Error", xlab="Categories", ylab="Error")
dev.off()

#print(eig.val)
#print(eig.val[0:20,1])

#print(eig.val[0:20,1])