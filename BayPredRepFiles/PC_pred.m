function pred = PC_pred(y,x,p,r,h);
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
% function pred = PC_pred(y,x,p,r,h);
% Compute h-step ahead prediction and coefficients from Lasso regression
% Y = F*gamma+e
% Y = (y_{+1}+...+y_{+h})/h; 
% F: r-principal components of X = [x x_{-1}... x_{-p}]
%
% Input:
% y:       dependent variable
% X:       predictors
% p:       lags of the predictors
% r:       number of principal components
% h:       number of steps ahead

% Output:
% pred:    h-step ahead predictions (\hat y_{T+h}+...+y_{T+1})/h 


temp=[];
%% Put togeteher predictors and their lags
%% X = [x x_{-1}... x_{-p}]
for j = 0:p
    temp = [temp x(p+1-j:end-j,:)];
end;
X = temp;
y = y(p+1:end,:);

%% Compute the principal components on standardized predictors
XX = center(X)*diag(1./std(X));
OPTS.dip = 0;
[F,d] = eigs(XX*XX',r,'lm',OPTS);

%% Regressors
Z = [ones(size(F(:,1))) F];


%% Compute the dependent variable to be predicted
%% Y = (y_{+1}+...+y_{+h})/h
Y = filter(ones(h,1)/h,1,y);

%% Compute the forecasts
gamma = inv(Z(1:end-h,:)'*Z(1:end-h,:))*Z(1:end-h,:)'*Y(h+1:end,:);
pred = Z(end,:)*gamma;