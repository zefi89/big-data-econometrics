require(glmnet)
require(signal)

glmnetPred <- function(y,x,p,lambda,h,alpha) {

    # Put togeteher predictors and their lags
    # X = [x x_{-1}... x_{-p}]

    for (j in 0:p) {
        if (j == 0) {
            temp <- x[(p+1):NROW(x),]
        } else {
            temp <- cbind(temp, x[(p+1-j):(NROW(x)-j),])
        }
    }
    X <- temp
    y <- y[(p+1):NROW(y),]

    # Normalize the regressors to have |X|<1
    nc <- NCOL(X)
    nr <- NROW(X)
    XX <- scale(X, center = TRUE, scale = TRUE)/sqrt(nc*nr)
    XX <- X
    # Regressors used for computing the regression coefficients
    Z <- XX[1:(NROW(XX)-h),]

    ## Massage the data a bit into the right format
    Z <- as.matrix(Z)

    # Compute the dependent variable to be predicted
    # Y = (y_{+1}+...+y_{+h})/h
    Y = filter(rep(1,h)/h,1,unlist(y))
    Y = Y[(h+1):length(Y)]

    # Standardize the dependent variable to have mean zero and unitary variance.
    # (Mean and variance will be reattributed to the forecsats, see below)
    my <- mean(Y)
    sy <- sd(Y)
    y_std <- (Y-my)/sy

    # Do the fit
    #b <- solve(t(Z) %*% Z + lambda*diag(nc)) %*% t(Z) %*% y_std
    reg.mod <- glmnet(Z, y_std, alpha=alpha, family='gaussian', lambda=lambda, intercept=FALSE)
    #print(coef(reg.mod))

    # Get the coefficients
    reg.coef <- coef(reg.mod)
    b <- reg.coef[2:NROW(reg.coef)]
    #intercept <- reg.coef[1]

    # Make the prediction
    Ztest <- as.matrix(XX[NROW(XX),])
    pred <- predict(reg.mod, newx=Ztest)*sy+my
    #pred <- 0

    # Get the MSE relative to the variance of Y
    #mse = var(Y - ((Z %*% b) + intercept))/var(Y)
    mse = var(y_std - (Z %*% b))

    returnlist <- list(pred, b, mse)
    names(returnlist) <- c("pred", "b", "mse")

    return(returnlist)

}

rwPred <- function(y,h) {

    # Compute the dependent variable to be predicted
    # Y = (y_{+1}+...+y_{+h})/h
    Y = filter(rep(1,h)/h,1,unlist(y))
    Y = Y[(h+1):length(Y)]

    return(mean(Y))
}
