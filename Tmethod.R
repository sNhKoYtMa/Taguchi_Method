#=========================================================#
##### description of code #####
# Taguchi's T-method main code
#=========================================================#
##### run only first time #####
Result <- matrix(NA,10,6) ; out <- 1
##### setting #####
source('data_artificial_linear.R') # data selection
time <- proc.time()
n.time <- SimTime
for(sim in 1:SimTime){
  ##### parameter set #####
  source('parameter_Tmethod.R')
  #=========================================================#
  ##### T-method calculation #####
  #=========================================================#
  ##### Step1: Determination of unit space #####
  no.unit <- order(y.train)[ceiling(length(y.train)/2)] # No. unit space
  signal.size <- train.size - 1 # number of signal data
  X.signal <- X.train[-no.unit,]            # X signal
  y.signal <- as.matrix(y.train[-no.unit])  # y signal
  X.unit <- t(as.matrix(X.train[no.unit,])) # X unit
  y.unit <- as.matrix(y.train[no.unit])     # y unit
  X.norm <- sweep(X.signal, 2, X.unit)      # X normalized
  y.norm <- sweep(y.signal, 2, y.unit)      # y normalized
  ##### Step2: Calculation of beta and eta for each item #####
  r <- as.numeric(t(y.norm) %*% y.norm) # effective divisor
  Beta <- t(t(X.norm) %*% y.norm) / r
  ST <- t(diag(t(X.norm) %*% X.norm))
  SB <- t((t(X.norm) %*% y.norm)) ** 2 / r
  SE <- ST - SB
  VE <- SE / signal.size
  eta <- (SB - VE) / (r * VE)
  eta[eta<0] <- 0
  ##### Step3: Calculation of an integrated estimated output value for each signal member #####
  ##### Step4: Calculation of the previously normalized overall estimated output value #####
  X.test.norm <- sweep(X.test, 2, X.unit)
  y.train.hat <- as.matrix(apply(X.norm, 1, function(X) {
    return(sum((eta/sum(eta))*X/Beta)+y.unit)}))
  y.test.hat <- as.matrix(apply(X.test.norm, 1, function(X) {
    return(sum((eta/sum(eta))*X/Beta)+y.unit)}))
  ##### Step5: Evaluation #####
  y.test <- as.matrix(y.test)
  error.test <- y.test.hat - y.test
  error.train <- y.train.hat - y.signal
  PE[sim] <- mean(error.test**2) # PE
  ERSS[sim] <- mean(error.train**2) # ERSS
  #=========================================================#
  ##### T-method END #####
  #=========================================================#
  i.time <- sim
  cat('\n',i.time,'/',n.time)
  cat(' Elapsed:', round(proc.time()[1]-time[1]), 's ')
  cat('Rest:',round((proc.time()[1]-time[1])/(i.time)*n.time-(proc.time()[1]-time[1])),'s ')
  cat('Total:',round((proc.time()[1]-time[1])/(i.time)*n.time),'s \n')
}
##### output of simulation's Result #####
Result[out,] <- cbind(train.size, e.sd, X.corr, mean(PE[!is.na(PE)]),
                      var(PE[!is.na(PE)]), mean(ERSS[!is.na(ERSS)]))
colnames(Result) <- c('size','var','corr','PE','VPE','ERSS')
write.csv(Result, 'output.csv', row.names=F)
out <- out + 1
Result
