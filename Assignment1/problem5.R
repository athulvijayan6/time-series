
# A general function to produce covariance matrix of a n dimensional random vector 
covariance <- function(X) {
    muVec <- colMeans(X);   # find mean of each random vector
    N <- 1:nrow(x);
    CovMatrix <- matrix(0, ncol(X), ncol(X));  #initialize to zero
    for (i in 1:ncol(X)) {   # loop through pair of every random vectors
        for (j in 1:ncol(X)) {
            for (k in N) {
                CovMatrix[i, j] = CovMatrix[i, j] + (X[k, i] - muVec[i])*(X[k, j]-muVec[j]); #keep adding
            }
            CovMatrix[i, j] = CovMatrix[i,j]/length(N); #divide by size
        }
    }
    return(CovMatrix);   #return the result
}

# create dataset
# sqrt since rnorm has argument standard deviation and given is variance

X <- rnorm(1000, 1, sqrt(3));
Y <- X^2 + 4*X +2;
x <- cbind(X, Y);

# covariance from my function
myCov = covariance(x);
# covariance from R
rCov = cov(x);

View(myCov);
View(rCov);