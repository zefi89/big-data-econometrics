##########################################################################
# Replications files for the paper:
#
# Forecasting using a large number of predictors:
# is Bayesian regression a valid alternative to principal components?
# Manuscript, ECARES-ULB, 2006
#
# Christine De Mol, Universite' Libre de Bruxelles and ECARES,
# Domenico Giannone, Universite' Libre de Bruxelles and ECARES,
# Lucrezia Reichlin, European Central Bank, ECARES and CEPR
#
# Programs and manuscript available at:
# http://homepages.ulb.ac.be/~dgiannon/
# http://homepages.ulb.ac.be/~lreichli/
##########################################################################
# [nu,b] = SET_ridge(y,x,p,INfit,h);
# Select the penalization parameter nu for which K predictors are kept in
# lasso regression
# Y = X*beta+e
# Y = (y_{+1}+...+y_{+h})/h;
# X = [x x_{-1}... x_{-p}]
#
# Input:
# y:       dependent variable
# X:       predictors
# p:       lags of the predictors
# INfit:   proportion of in-sample fit to be explained by ridge
# h:       number of steps ahead
#
# Output:
# nu: selected Penalization Parameter
# b : Regression coefficients (on standardized data)

source("GlmnetWrapper.R")

setRidge <- function(y,x,p,INfit,h,alpha) {


    nu_min <- 0
    nu_max <- 10000
    IN_max <- 1e+32
    IN_min <- 0
    IN_avg <- 1e+32

    while (abs(IN_avg-INfit)>1e-7) {

        nu_avg <- (nu_min+nu_max)/2

        fit <- glmnetPred(y, x, p, nu_avg, h, alpha)

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
