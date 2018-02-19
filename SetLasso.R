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
#function [nu,b] = SET_lasso(y,x,p,K,h);
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
# K:       Number of predictors to keep
# h:       number of steps ahead
#
# Output:
# nu: selected Penalization Parameter
# b : Regression coefficients (on standardized data) 

function [nu,b] = SET_lasso(y,x,p,K,h);

nu_min = 0;
nu_max = 2;
K_max = 1e+32;
K_min = 0;
K_avg = 1e+32;
max_iter = 30;

cont = 0;

while K_min~=K_max & cont<max_iter
    cont = cont + 1;
    nu_avg = (nu_min+nu_max)/2;
    [pred,b] = LASSO_pred(y,x,p,nu_avg,h);
    K_avg = sum(b(:,end)~=0);
        
    if K_avg < K
        nu_min = nu_min;
        nu_max = nu_avg;
    else
        nu_min = nu_avg;
        nu_max = nu_max;
    end;
    
end;


if cont>=max_iter
    message('warning: max iter reached vwhen setting the Lasso penalization')
end

nu = nu_avg;
b = b(:,end);
