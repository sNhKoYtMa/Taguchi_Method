X.train <- data[1:train.size, 2:(X.dim+1), sim] # train data of X
y.train <- data[1:train.size, 1, sim]           # train data of y
X.test  <- data[(train.size+1):(train.size+test.size), 2:(X.dim+1), sim] # test data of X
y.test  <- data[(train.size+1):(train.size+test.size), 1, sim]           # test data of y
dimnames(X.train) <- NULL ; dimnames(X.test) <- NULL
dimnames(y.train) <- NULL ; dimnames(y.test) <- NULL
X.unit <- matrix(0, 1, X.dim) # X average
y.unit <- matrix(0, 1, 1)     # y average
X.norm <- matrix(0, train.size, X.dim)   # X normalized
y.norm <- matrix(0, train.size, 1)       # y normalized
X.test.norm <- matrix(0, test.size, X.dim)   # normalized X.test
y.test.norm <- matrix(0, test.size, 1)       # normalized y.test
r <- 0                       # effective divisor
Beta <- matrix(0, 1, X.dim)  # proportion term
ST <- matrix(0, 1, X.dim)    # total variation
SB <- matrix(0, 1, X.dim)    # variation of proportional term
SE <- matrix(0, 1, X.dim)    # error variation
VE <- matrix(0, 1, X.dim)    # error variance
eta <- matrix(0, 1, X.dim)   # SN ratio
y.train.hat <- matrix(0,train.size,1) # prediction of y.train 
y.test.hat <- matrix(0,test.size,1)   # prediction of y.test
error.test <- matrix(NA, test.size, 1)      # errors of test data
error.train <- matrix(NA, train.size-1, 1)    # errors of train data