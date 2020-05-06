#=========================================================#
##### description of code #####
# artificial data in simulation
# linear multiple regression model
#=========================================================#
set.seed(722)      # seed of random number (fixed)
##### Parameter #####
SimTime <- 1000    # number of simulation
train.size <- 10   # size of train data
e.sd <- 1        # standard deviation of errors
X.corr <- 0.0      # X correlation
test.size <- 100   # size of test data
X.dim <- 5         # data dimention
PE <- matrix(NA,SimTime,1)    # PE (Prediction Error)
ERSS <- matrix(NA,SimTime,1)  # ERSS (Expectated Residual Sum of Squares)

##### function definition #####
library(mvtnorm)
make.regression.data <- function(train,test,dim,corr,sd){
  b <- matrix(c(1,1,1,1,1))      # coefficient
  mu <- c(1,1,1,1,1)             # means of X
  sigma <- matrix(corr,dim,dim)  # correlation matrix of 
  X <- matrix(NA,(train+test),dim)
  for(i in 1:dim) for(j in 1:dim) sigma[i,j] <- sigma[i,j]^abs(i-j) # covariance matrix
  X <- matrix(rmvnorm((train+test),mu,sigma), (train+test), dim)
  e <- rnorm((train+test),mean=0,sd=sd) # white noise
  y <- X%*%b + e
  return(cbind(y,X))
}
data <- array(NA,dim=c(train.size+test.size,X.dim+1, SimTime))
for(i in 1:SimTime) data[,,i] <- make.regression.data(train.size, test.size, X.dim, X.corr, e.sd)
