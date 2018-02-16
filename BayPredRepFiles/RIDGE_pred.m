function [pred,b,MSE] = RIDGE_pred(y,x,p,nu,h);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Replications files for the paper:
%
% Forecasting using a large number of predictors: 
% is Bayesian regression a valid alternative to principal components?
% Manuscript, ECARES-ULB, 2006 
% 
% Christine De Mol, Universite' Libre de Bruxelles and ECARES,
% Domenico Giannone, Universite' Libre de Bruxelles and ECARES,
% Lucrezia Reichlin, European Central Bank, ECARES and CEPR
%
% Programs and manuscript available at: 
% http://homepages.ulb.ac.be/~dgiannon/
% http://homepages.ulb.ac.be/~lreichli/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [pred,b,MSE] = RIDGE_pred(y,x,p,nu,h);
% Compute h-step ahead prediction and coefficients from Ridge regression
% Y = X*beta+e
% Y = (y_{+1}+...+y_{+h})/h; 
% X = [x x_{-1}... x_{-p}]
%
% Input:
% y:       dependent variable
% X:       predictors
% p:       lags of the predictors
% nu:      penalization parameters
% h:       number of steps ahead
%
% Output:
% pred:    h-step ahead predictions (\hat y_{T+h}+...+y_{T+1})/h 
% b:       estimates regression coeffiencients (on standardized data)
% MSE:     In-sample variance explained by the regression.

temp=[];
%% Put togeteher predictors and their lags
%% X = [x x_{-1}... x_{-p}]
for j = 0:p
    temp = [temp x(p+1-j:end-j,:)];
end;
X = temp;
y = y(p+1:end,:);

%% Normalize the regressors to have |X|<1
[T,N] = size(X);
XX = center(X)*diag(1./std(X))/(sqrt(N*T));
%% Regressors used for computing the regression coefficients
Z = [XX(1:end-h,:)];


%% Compute the dependent variable to be predicted
%% Y = (y_{+1}+...+y_{+h})/h
Y = filter(ones(h,1)/h,1,y);
Y = Y(h+1:end,:);

%% Standardize the dependent variable to have mean zero and unitary variance.
%% (Mean and variance will be reattributed to the forecsats, see below)
my = mean(Y);
sy = std(Y);
y_std = (Y-my)/sy;

b = inv(Z'*Z + nu*eye(N))*Z'*y_std;

pred = (XX(end,:)*b)*sy+my; 

MSE = var(y_std-Z*b);