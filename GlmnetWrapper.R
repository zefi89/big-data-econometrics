require(glmnet)
require(signal)

glmnetPred <- function(y,x,p,nu,h,alpha) {

    # Put togeteher predictors and their lags
    # X = [x x_{-1}... x_{-p}]

    for (j in 0:p) {
        if (j == 0) {
            temp <- x[(p+1):NROW(x),]
        } else {
            print(dim(x[(p+1-j):(NROW(x)-j),]))
            temp <- cbind(temp, x[(p+1-j):(NROW(x)-j),])
        }
    }
    X <- temp
    y <- y[(p+1):NROW(y),]

    # Regressors used for computing the regression coefficients
    Z <- X[1:(NROW(X)-h),]

    ## Massage the data a bit into the right format
    Z <- as.matrix(Z)

    # Compute the dependent variable to be predicted
    # Y = (y_{+1}+...+y_{+h})/h
    Y = filter(rep(1,h)/h,1,unlist(y))
    Y = Y[(h+1):length(Y)]

    reg.mod <- glmnet(Z, Y, alpha=alpha, lambda=0.1)

    return(reg.mod)

}


#function [pred,b,MSE] =
