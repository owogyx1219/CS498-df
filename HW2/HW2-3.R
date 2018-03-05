{
# setwd("/Users/ximinlin/Documents/UIUC_Basic/2018 Spring/CS498AML/HW2/")
# getwd()
  df1 = read.table("adult.data.txt",header=FALSE,sep=",")
  df2 = read.table("adult.test.txt",header=FALSE,sep=",")
  df = rbind(df1,df2)
  bigY = df[,c(15)]
  bigY = lapply(as.character(bigY), function(x){if(x==" >50K"){ 1}else{-1} }) # convert to +1/-1 vector
  bigY = unlist(bigY, use.names = FALSE)
  bigX = as.matrix(df[,c(1,3,5,11,12,13)])
  n_samples = dim(bigX)[1]
  n_feat = dim(bigX)[2]
  # scale to unit variance
  n_sd = apply(bigX, 2, sd, na.rm=TRUE)
  bigX = t(t(bigX)/n_sd)
  # partition data
  trposflag = createDataPartition(bigY, times = 1, p = 0.8, list = FALSE)
  trX = bigX[trposflag,]
  trY = bigY[trposflag]
  cvposflag = createDataPartition(bigY[-trposflag], times = 1, p = 0.5, list = FALSE)
  dim(trX)
  dim(bigX)
  cvX = bigX[cvposflag,]
  cvY = bigY[cvposflag]
  teX = bigX[-c(trposflag, cvposflag),]
  teY = bigY[-c(trposflag, cvposflag)]
}

# lambda values to be chosen
lamlist = c(1e-4,1e-3,1e-2,1e-1,1)
# validation accuracy list
cvAcc = array(0,dim=length(lamlist))

svm_acc = function(ntrX, ntrY){
  pred = ntrX%*%a + b
  ppred = pred > 0
  pntrY = ntrY > 0
  mean(ppred == pntrY)
}

get_abs_a = function(){
  sqrt(sum(a^2))
}

m = 1.0
n = 50.0
evacc = array(0,dim=c(length(lamlist),300/30*50) )
cfmag = array(0,dim=c(length(lamlist),300/30*50) )

# iterate through lambda
for(lam_idx in 1:length(lamlist) ){
  lam = lamlist[lam_idx]
  # intialize a and b to be random numbers in normal distribution with mean 0 and sd 1
  a = rnorm(n_feat+1, 0, 1)
  b = a[n_feat+1]
  a = a[-(n_feat+1)]
  dim(a) = c(n_feat,1)
  # train and plot
    # 50 epochs
    for(e_idx in 0:49){
      # randomly pick 50 samples from the training for evaluation, train on the rest
      evposflag = sample(1:nrow(trX), 50)
      ntrX = trX[evposflag,]  # not used for training
      ntrY = trY[evposflag]
      rtrX = trX[-evposflag,] # remained 
      rtrY = trY[-evposflag]
      # 300 steps
      for(step in 1:300){
        # every 30 steps
        if(step %% 30 == 0){
            # find accuracy and plot
            evacc[lam_idx, e_idx*10+step/30] = svm_acc(ntrX, ntrY)
            # find magnitude of the coeff vector and plot
            cfmag[lam_idx, e_idx*10+step/30] = get_abs_a()
        }
        # randomly pick 100 samples from the training to calculate gradient
        trposflag = sample(1:nrow(rtrX),100)
        btrX = trX[trposflag,]
        btrY = trY[trposflag]
        # calculate gradients
        # posflag_grad = btrY*(btrX%*%a + b) < 1
        # grad_a = -btrY*btrX + matrix(rep(lam*a, each=dim(btrX)[1]),nrow=dim(btrX)[1]) # gradient + regularization
        # grad_b = -btrY
        grad_a = array(0,dim=dim(btrX)[2])
        grad_b = 0
        for(sm_idx in 1:nrow(btrX)){
          sm_Y = btrY[sm_idx]
          sm_X = btrX[sm_idx,]
          if(sm_Y*(sm_X%*%a + b) < 1){
            grad_a = grad_a + -sm_Y*sm_X
            grad_b = grad_b - sm_Y
          }
        }
        grad_a = grad_a / dim(btrX)[1]; dim(grad_a) = c(6,1)
        grad_b = grad_b / dim(btrX)[1]
        # if(sum(posflag_grad) == 0){
        #   dir_a = 0
        # }
        # else if(sum(posflag_grad) == 1){
        #   dir_a = -grad_a[posflag_grad,]
        # }
        # else{
        #   dir_a = colSums(-grad_a[posflag_grad,])
        # }
        # dir_b = sum(-grad_b[posflag_grad])
        # update a b
        up_a = grad_a + lam*a
        up_b = grad_b
        a = a - m/(n+0.01*e_idx)*up_a
        b = b - up_b
      }
    }
  # validate
  cat("for lambda = ",lam,"validation accuracy = ", svm_acc(cvX, cvY),"\n")
}

colors = c('blue','red','yellow','black','green')

# plotting evaluation accuracy graph
{
  plot(evacc[1,], type='l', col = 'blue', ylim = c(0,1))
  for(i in 2:dim(evacc)[1]){
    lines(evacc[i,], type='l', col = colors[i])
  }
  legend(x = 0, y = 0.6, legend = paste("lambda = ",lamlist,sep = ""), col=colors, lty=1:2, cex=0.8)
}

# plotting magnitude of a graph
{
  plot(cfmag[1,], type='l', col='blue', ylim = c(0,2))
  for(i in 2:dim(cfmag)[1]){
    lines(cfmag[i,], type='l', col = colors[i])
  }
  legend(x = 150, y = 2.0, legend = paste("lambda = ",lamlist,sep=""), col=colors, lty=1:2, cex=0.8)
}


# final training on cv and tr combined
flam = 1e-4
{
  ftrX = rbind(trX,cvX)
  ftrY = c(trY,cvY)
  # for 50 epochs
  for(e_idx in 0:49){
    # 300 steps
    for(step in 1:300){
      # randomly pick 100 samples from the training to calculate gradient
      trposflag = sample(1:nrow(ftrX),100)
      btrX = ftrX[trposflag,]
      btrY = ftrY[trposflag]
      # calculate gradients
      # posflag_grad = btrY*(btrX%*%a + b) < 1
      # grad_a = -btrY*btrX + matrix(rep(lam*a, each=dim(btrX)[1]),nrow=dim(btrX)[1]) # gradient + regularization
      # grad_b = -btrY
      grad_a = array(0,dim=dim(btrX)[2])
      grad_b = 0
      for(sm_idx in 1:nrow(btrX)){
        sm_Y = btrY[sm_idx]
        sm_X = btrX[sm_idx,]
        if(sm_Y*(sm_X%*%a + b) < 1){
          grad_a = grad_a + -sm_Y*sm_X
          grad_b = grad_b - sm_Y
        }
      }
      grad_a = grad_a / dim(btrX)[1]; dim(grad_a) = c(6,1)
      grad_b = grad_b / dim(btrX)[1]
      # update a b
      up_a = grad_a + flam*a
      up_b = grad_b
      a = a - m/(n+0.01*e_idx)*up_a
      b = b - up_b
    }
  }
}

# test
{
  fteaccy = svm_acc(ntrX = teX, ntrY = teY)
  cat("final testing accuracy is", fteaccy,"\n")
}




