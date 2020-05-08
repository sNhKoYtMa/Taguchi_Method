#=========================================================#
##### description of code #####
# MSR(Multiple Single Regression) main code
#=========================================================#
##### run only first time #####
# Result <- matrix(NA,10,7) ; out <- 1
##### setting #####
source('data_artificial_linear.R') # data selection
delta <- 0.01 # residual threshold delta
time <- proc.time()
n.time <- SimTime
for(sim in 1:SimTime){
  ##### parameter set #####
  source('parameter_MSR.R')
  #=========================================================#
  ##### MSR calculation #####
  #=========================================================#
  ##### standardize data #####
  X.mean <- t(apply(X.train, 2, mean))
  X.train.std <- sweep(X.train, 2, X.mean)
  X.test.std <- sweep(X.test, 2, X.mean)
  y.train.std <- y.train - mean(y.train)
  y.test.std <- y.test - mean(y.train)
  
  ##### Step 1：beta.hat=0 #####
  beta.hat <- matrix(0, X.dim, 1)
  beta.hat.cv <- array(0,dim=c(X.dim, 1, train.size))
  ##### initial value #####
  count <- 1  # calculation time
  judge <- matrix(0, train.size, 1) # 0 or 1 (if stop judgement is true)
  
  ##### repeat until stop rule is OK #####
  while(1){
    ##### Step 2：y.hat=X%*%beta.hat #####
    y.hat <- X.train.std%*%beta.hat
    
    ##### Step 3：z=y-y_hat #####
    z <- y.train.std - y.hat
    
    ##### Stop rule 1：residuals convergence #####
    while(1){
      for(cv.no in 1:train.size){
        X.cv <- X.train.std[-cv.no,]  # X data, cross varidation
        y.cv <- y.train.std[-cv.no]   # y data, cross varidation
        y.hat.cv <- X.cv %*% beta.hat.cv[,,cv.no] # predict
        z.cv <- y.cv - y.hat.cv  # z  [Step 3]
        ST <- t(z.cv) %*% z.cv   # ST [Step 4]
        gammaSb <- apply(X.cv, 2, function(X){
          XX <- t(X)%*%X
          zX <- t(z.cv)%*%X
          if(XX >= 10^(-10)){
            return(c(zX / XX, zX**2 / XX))
          }else return(c(0,0))
        })
        gamma <- t(gammaSb[1,]) # gamma
        Sb <- t(gammaSb[2,])    # Sb
        if(ST>=(10^(-10)) && sum(abs(Sb))>=(10^(-10))){
          eta <- t(apply(Sb, 2, function(SB){sqrt(SB/(ST-SB))})) # update eta
          eta[eta>1000] <- 1000 # eta = 1000
          sum.e <- sum(eta)
          weight <- t(apply(eta, 2, function(e){e/sum.e})) # calculate w (weight)
        } else{
          judge[cv.no] <- 1 # if judge = 1; alpha - 0
        }
        alpha <- t(weight*gamma) # alpha
        if(judge[cv.no]==1) alpha <- 0
        beta.hat.cv[,,cv.no] <- beta.hat.cv[,,cv.no] + alpha # beta := beta + alpha [Step 5]
        y.hat.2[cv.no] <- t(X.train.std[cv.no,]) %*% beta.hat.cv[,,cv.no] # y_hat=x*β
        z.22[cv.no] <- y.train.std[cv.no] - t(X.train.std[cv.no,]) %*% beta.hat.cv[,,cv.no] # z=y-y_hat
      }
      if(count > 1) break
      z.21 <- z.22
      count <- count + 1 # count+1, if only count=1
    }
    if( (t(z.21)%*%z.21 - t(z.22)%*%z.22) <= (t(z.21)%*%z.21*delta)) break
    # Stop (if residuals' update is convergenced.)
    count <- count + 1 # count+1
    z.21 <- z.22
    
    ##### Step 4 #####
    ST <- t(z)%*%z  # calculate ST
    
    ##### Stop rule 2 #####
    gammaSb <- apply(X.train.std, 2, function(X){
      XX <- t(X)%*%X
      zX <- t(z)%*%X
      if(XX >= 10^(-10)){
        return(c(zX / XX, zX**2 / XX))
      }else return(c(0,0))
    })
    gamma <- t(gammaSb[1,])
    Sb <- t(gammaSb[2,])
    
    ##### Stop rule 3 #####
    if(ST<=10^(-10)) break
    if (sum(Sb)<=50^(-10)) break
    eta <- t(apply(Sb, 2, function(SB){sqrt(SB/(ST-SB))})) # update eta
    eta[eta>1000] <- 1000 # eta = 1000
    sum.e <- sum(eta)
    weight <- t(apply(eta, 2, function(e){e/sum.e})) # calculate w (weight)
    alpha <- t(weight*gamma) # calculate alpha
    z <- X.train.std%*%alpha # calculate residuals
    ##### Step 5：update beta#####
    beta.hat <- beta.hat + alpha
  }
  
  ##### PE #####
  y.test.hat <- X.test.std%*%beta.hat + mean(y.train)
  error.test <- y.test.hat - y.test
  PE[sim] <- mean(error.test^2) # PE
  
  ##### ERSS #####
  y.train.hat <- X.train.std%*%beta.hat + mean(y.train)
  error.train <- y.train.hat - y.train
  ERSS[sim] <- mean(error.train^2) # ERSS
  
  #=========================================================#
  ##### MSR(Multiple Single Regression) END #####
  #=========================================================#
  i.time <- sim
  cat('\n',i.time,'/',n.time)
  cat(' Elapsed:', round(proc.time()[1]-time[1]), 's ')
  cat('Rest:',round((proc.time()[1]-time[1])/(i.time)*n.time-(proc.time()[1]-time[1])),'s ')
  cat('Total:',round((proc.time()[1]-time[1])/(i.time)*n.time),'s ')
  cat('count:', count, '\n')
}
##### output of simulation's Result #####
Result[out,] <- cbind(train.size, e.sd, X.corr, delta, mean(PE[!is.na(PE)]),
                      var(PE[!is.na(PE)]), mean(ERSS[!is.na(ERSS)]))
colnames(Result) <- c('size','var','corr','delta','PE','VPE','ERSS')
write.csv(Result, 'output.csv', row.names=FALSE)
out <- out + 1
Result