# Generate the given series
N <- 1500;
v <- arima.sim(n=1500, model=list(ar=c(0.4), ma=c(0, 0.25)));
# Find the ACVF estimate
acvf <- acf(v, lag.max=N-1, type=covariance);


# part (2)
# Fit ts model
model = arima(v)