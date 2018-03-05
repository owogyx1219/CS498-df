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
    if(i== 1)
    {
      print(dim(data.frame(r, g, b)))
      print(head(data.frame(r,g,b)))
    }
    
    images.rgb[[index]] = c(r, g, b)
    
    if(i==1)
    {
      print(length(images.rgb[[index]]))
      print(images.rgb[[index]][1:10])
    }
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
    images.rgb[[index]] = c(t(data.frame(r, g, b)))
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

#print(length(images.rgb.1))
#print(dim(images.rgb.1[[1]]))

#value_1 = prcomp(as.data.frame(images.rgb.1),scale=FALSE, rank. = 20)
#print(value_1$rotation)

#print(eig.val)
#print(eig.val[0:20,1])

#print(eig.val[0:20,1])