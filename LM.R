#=========================================================#
##### description of code #####
# linear multiple regression main code
#=========================================================#
##### run only first time #####
Result <- matrix(NA,10,6) ; out <- 1
##### setting #####
source('data_artificial_linear.R') # data selection
time <- proc.time()
n.time <- SimTime
for(sim in 1:SimTime){
  ##### Calculation #####
  X.train <- data[1:train.size,2:(X.dim+1),sim] # train data of X
  y.train <- data[1:train.size,1,sim]           # train data of y
  X.test  <- data[(train.size+1):(train.size+test.size),2:(X.dim+1),sim] # test data of X
  y.test  <- data[(train.size+1):(train.size+test.size),1,sim]           # test data of y
  dimnames(X.train) <- NULL ; dimnames(X.test) <- NULL
  dimnames(y.train) <- NULL ; dimnames(y.test) <- NULL
  X.train <- cbind(matrix(1,train.size,1),X.train)
  X.test <- cbind(matrix(1,test.size,1),X.test)
  beta.hat <- solve(t(X.train)%*%X.train)%*%t(X.train)%*%y.train
  ##### Evaluation #####
  y.test.hat <- X.test %*% beta.hat
  y.train.hat <- X.train %*% beta.hat
  error.test <- y.test.hat - y.test
  error.train <- y.train.hat - y.train
  PE[sim] <- mean(error.test^2) # PE
  ERSS[sim] <- mean(error.train^2) # ERSS
  i.time <- sim
  cat('\n',i.time,'/',n.time)
  cat(' Elapsed:', round(proc.time()[1]-time[1]), 's ')
  cat('Rest:',round((proc.time()[1]-time[1])/(i.time)*n.time-(proc.time()[1]-time[1])),'s ')
  cat('Total:',round((proc.time()[1]-time[1])/(i.time)*n.time),'s \n')
}
##### output of simulation's Result #####
Result[out,] <- cbind(train.size, e.sd, X.corr, mean(PE[!is.na(PE)]),
                      var(PE[!is.na(PE)]), mean(ERSS[!is.na(ERSS)]))
colnames(Result) <- c('size','Var','corr','PE','VPE','ERSS')
write.csv(Result, 'output.csv', row.names=FALSE)
out <- out + 1
Result