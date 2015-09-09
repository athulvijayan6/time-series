# Function calculates PACF of time series V[k] upto L lags using Durbin-Levinson algorithm



mypacf <- function(V, L) {
    ACF <- acf(V, lag.max = L, plot = FALSE);
    Phi = matrix(data=NA, nrow=L, ncol=L);
    PACF = matrix(data=NA, nrow=L, ncol=1);
    Phi[1,1] <- ACF[0];
    PACF[1,1] <- Phi[1, 1];

    for (i in 1:L-1){
        sum1 = 0;
        sum2 = 0;
        for (j in 1:i) {
            sum1 <- sum1 + (Phi[i, j]*ACF[i+1-j, 1]);
            sum2 <- sum2 + (Phi[i, j]*ACF[j, 1]);
        }
        Phi[i+1, i+1] <- (ACF[i+1, 1] - sum1)/(1-sum2);
        for (j in 1:i) {
            Phi(i+1, j) <- Phi[i, j] - (Phi[i+1, i+1]*Phi[i, i-j+1]);
        }
        PACF[i+1, 1] <- Phi[i+1, i+1];
    }
    return(PACF)

}

load('a3_q1.Rdata');
pacf <- mypacf(xk, 100);
plot(pacf);