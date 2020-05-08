#=========================================================#
##### description of code #####
# variable definitions and descriptions of MSR
#=========================================================#
X.train <- data[1:train.size,2:(X.dim+1),sim] # train data of X
y.train <- as.matrix(data[1:train.size,1,sim])           # train data of y
X.test  <- data[(train.size+1):(train.size+test.size),2:(X.dim+1),sim]  # test data of X
y.test  <- as.matrix(data[(train.size+1):(train.size+test.size),1,sim]) # test data of y
dimnames(X.train) <- NULL ; dimnames(X.test) <- NULL
dimnames(y.train) <- NULL ; dimnames(y.test) <- NULL
X.mean <- matrix(NA, 1, X.dim)              # means of X.train
X.train.std <- matrix(0, train.size, X.dim) # standardized X.train
y.train.std <- matrix(0, train.size, 1)     # standardized y.train
X.test.std <- matrix(0, test.size, X.dim)   # standardized X.test
y.test.std <- matrix(0, test.size, 1)       # standardized y.test
X.cv <- matrix(0, train.size-1, X.dim)      # cross validation x
y.cv <- matrix(0, train.size-1, 1)          # cross validation y
y.hat <- matrix(0, train.size, 1)           # y.hat
y.hat.2 <- matrix(0, train.size, 1)        # 2nd y.hat
y.hat.cv <- matrix(0, train.size-1, 1)      # cross validation y.hat
z <- matrix(0,train.size, 1)                # residuals
z.21 <- matrix(0, train.size, 1)            # z.hat 1
z.22 <- matrix(0, train.size, 1)            # z.hat 2
z.cv <- matrix(0, train.size-1, 1)          # z of cv
gammaSb <- matrix(0, 2, X.dim)              # gamma and Sb
gamma <- matrix(0, 1, X.dim)                # gamma (minimize Sej)
alpha <- matrix(0, X.dim, 1)                # alpha (update beta)
y.train.hat <- matrix(0,train.size,1)       # prediction of y.train 
y.test.hat <- matrix(0,test.size,1)         # prediction of y.test
XX <- matrix(0, 1, 1)
zX <- matrix(0, 1, 1)
ST <- matrix(0, 1, 1)
Sb <- matrix(0, 1, X.dim)                   # Sb
eta <- matrix(0, 1, X.dim)                  # eta
weight <- matrix(0, 1, X.dim)               # weight
error.test <- matrix(NA, test.size, 1)      # errors of test data
error.train <- matrix(NA, train.size, 1)    # errors of train data
