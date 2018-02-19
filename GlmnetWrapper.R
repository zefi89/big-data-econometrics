require(glmnet)
require(signal)

glmnetPred <- function(y,x,p,nu,h,alpha) {

    # Put togeteher predictors and their lags
    # X = [x x_{-1}... x_{-p}]

    temp <- x[1:NROW(x),]
    for (j in 0:p) {
        if (j == 0) {
            temp <- x[1:NROW(x),]
        } else {
            temp <- rbind(temp, x[(p+1-j):(NROW(x)-j),])
        }
    }
    X <- temp
    y <- y[p+1:NROW(y),]

    # Regressors used for computing the regression coefficients
    Z <- X[1:(NROW(X)-h),]

    ## Massage the data a bit into the right format
    y <- as.numeric(unlist(y))
    Z <- as.matrix(Z)

    # Compute the dependent variable to be predicted
    # Y = (y_{+1}+...+y_{+h})/h
    y <- as.numeric(unlist(y))
    Y = filter(rep(1,h)/h,1,y)
    Y = as.numeric(unlist(Y))
    Y = Y[h+1:length(Y)]

    #print(Z)
    #print(Y)
    print(dim(Z))
    print(length(Y))

    reg.mod <- glmnet(Z, Y, alpha=alpha, lambda=1)

    return(reg.mod)

}


#function [pred,b,MSE] =
