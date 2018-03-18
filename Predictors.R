require(glmnet)
require(signal)
require(rpart)
require(randomForest)

############################
# Lasso or ridge with glmnet
############################

glmnetPred <- function(y,x,p,lambda,h,alpha, cv=FALSE, cv_plot_file='') {

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
    XX <- scale(X, center = TRUE, scale = TRUE)#/sqrt(nc*nr)
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
    if (cv) {
        if (alpha == 0) {
           lamseq <- exp(log(10)*seq(log10(0.01),log10(1000),by=0.005))
        }
        if (alpha == 1) {
           lamseq <- exp(log(10)*seq(log10(0.01),log10(0.5),by=0.0005))
        }
        cvout <- cv.glmnet(Z, y_std, alpha=alpha, lambda=lamseq)
        if (cv_plot_file != '') {
            pdf(cv_plot_file)
            plot(cvout, ylim=c(0.5,1.5))
            dev.off()
        }
        lambda = cvout$lambda.min
    }
    reg.mod <- glmnet(Z, y_std, alpha=alpha, family='gaussian', lambda=lambda, intercept=FALSE)

    # Get the coefficients
    reg.coef <- coef(reg.mod)
    b <- reg.coef[2:NROW(reg.coef)]

    # Make the prediction
    Ztest <- t(as.matrix(XX[NROW(XX),]))
    pred <- predict(reg.mod, newx=Ztest)*sy+my

    # Get the MSE relative to the variance of Y
    mse = var(y_std - (Z %*% b))

    returnlist <- list(pred, b, mse, lambda)
    names(returnlist) <- c("pred", "b", "mse", "lambda")

    return(returnlist)

}

#################
# Regression Tree
#################

rpartPred <- function(y,x,p,h,cvplot=FALSE, cv_plot_file='plot.pdf', random_forest=FALSE) {

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
    XX <- X
    XX[,] = scale(X, center = TRUE, scale = TRUE)#/sqrt(nc*nr)
    # Regressors used for computing the regression coefficients
    Z <- XX[1:(NROW(XX)-h),]
    Ztest <- XX[NROW(XX),]

    # Compute the dependent variable to be predicted
    # Y = (y_{+1}+...+y_{+h})/h
    Y = filter(rep(1,h)/h,1,unlist(y))
    Y = Y[(h+1):length(Y)]

    # Standardize the dependent variable to have mean zero and unitary variance.
    # (Mean and variance will be reattributed to the forecsats, see below)
    my <- mean(Y)
    sy <- sd(Y)
    y_std <- (Y-my)/sy

    Z$y_std <- y_std

    if (random_forest) {
        fit <- randomForest(y_std ~ .-y_std, data=Z)
        pred <- predict(fit, Ztest, type="response")*sy+my
    } else {

        fit <- rpart(y_std ~ .-y_std, method="anova", data=Z)#, control=rpart.control(minsplit = 20))
        pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

        #post(fit, file = "tree.ps")
        #post(pfit, file = "tree_pruned.ps")

        #print(pfit$cptable[,"xerror"])
        #printcp(fit) # display the results
        #summary(fit) # detailed summary of splits
        if (cvplot) {
            pdf(cv_plot_file)
            plotcp(fit) # visualize cross-validation results
            dev.off()
        }

        # Make the prediction
        pred <- predict(pfit, Ztest, type="vector")*sy+my
        }

    return(pred)
}

#############
# Random walk
#############

rwPred <- function(y,h) {

    # Compute the dependent variable to be predicted
    # Y = (y_{+1}+...+y_{+h})/h
    Y = filter(rep(1,h)/h,1,unlist(y))
    Y = Y[(h+1):length(Y)]

    return(mean(Y))
}

#####################
# Pricipal components
#####################

pcPred <- function(y,x,p,r,h) {

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

    # Centering is necessary for PRR
    XX <- scale(X, center = TRUE, scale = TRUE)
    XX <- as.matrix(XX)
    # Compute the principal components on standardized predictors
    eigs = eigen(XX %*% t(XX), symmetric=TRUE)
    Z <- as.matrix(eigs$vectors[,1:r])
    Ztrain <- as.matrix(Z[1:(NROW(Z)-h),])

    # Compute the dependent variable to be predicted
    # Y = (y_{+1}+...+y_{+h})/h
    Y = filter(rep(1,h)/h,1,unlist(y))

    # Standardize the dependent variable to have mean zero and unitary variance.
    # (Mean and variance will be reattributed to the forecsats, see below)
    my <- mean(Y)
    sy <- 1.#sd(Y)
    y_std <- (Y-my)/sy

    # Compute the forecasts
    gamma = solve(t(Ztrain) %*% Ztrain) %*% t(Ztrain) %*% y_std[(h+1):NROW(Y)]
    pred <- (t(Z[NROW(Z),]) %*% gamma) * sy + my

    return(pred)
}

#############################################################################################
# Helper function to set ridge parameter to match a franction of explained in-sample variance
#############################################################################################

setRidge <- function(y,x,p,INfit,h) {


    nu_min = 0
    nu_max = 10000
    IN_max = 1e+32
    IN_min = 0
    IN_avg = 1e+32

    while (abs(IN_avg-INfit)>1e-7) {

        nu_avg <- (nu_min+nu_max)/2

        fit <- glmnetPred(y, x, p, nu_avg, h, alpha=0)

        IN_avg = fit$mse

        if (IN_avg > INfit) {
            nu_min = nu_min
            nu_max = nu_avg
        }
        else {
            nu_min = nu_avg
            nu_max = nu_max
        }
    }

    returnlist <- list(nu_avg, fit$b)
    names(returnlist) <- c("lambda", "b")

    return(returnlist)

}

#########################################################################################
# Helper function to set lasso parameter to result in a number K of non-zero coefficients
#########################################################################################

setLasso <- function(y,x,p,K,h) {

    nu_min = 0
    nu_max = 2
    K_avg = 1e+32
    max_iter = 30

    cont = 0

    while (K_avg != K && cont < max_iter) {
        cont = cont + 1
        nu_avg = (nu_min+nu_max)/2

        fit <- glmnetPred(y, x, p, nu_avg, h, alpha=1)
        K_avg = sum(fit$b != 0)

        if (K_avg < K) {
            nu_min = nu_min
            nu_max = nu_avg
        }
        else {
            nu_min = nu_avg
            nu_max = nu_max
        }
    }

    if (cont>=max_iter ) {
        message('warning: max iter reached vwhen setting the Lasso penalization')
    }

    returnlist <- list(nu_avg, fit$b)
    names(returnlist) <- c("lambda", "b")

    return(returnlist)

}
