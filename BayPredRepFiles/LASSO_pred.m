function [pred,b] = LASSO_pred(y,x,p,nu,h,b_init);
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
% [pred,b] = LASSO_pred(y,x,p,nu,h,b_init);
% Compute h-step ahead prediction and coefficients from Lasso regression
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
% b_init:  initialization for the regression coefficients (zeros by default)
%
% Output:
% pred:    h-step ahead predictions (\hat y_{T+h}+...+y_{T+1})/h 
% b:       estimates regression coeffiencients (on standardized data)


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


%% Initialize the Iterative Landweber algorithm
thresh      = nu/2;       %% set the threshold 
tollerance  = 1e-5;       %% Tollerance for checking convergence
max_iter    = 10000;       %% Maximum number of iterations
cont        = 1;          %% Counts the number of iterations
pred(1)     = 0;          %% Intial value for the in-sample fit
fit_prev    = 1e+32;      %% Initialize the in sample fit
Dfit        = 1e+32;      %% Initial value for the change in fit, to check convergence;

%% Initialize the parameters
if nargin == 6 %% If initial parameters are provided 
    b(:,1) = b_init;
else %% set to zero initial values of the parameters by default
    b(:,1) = zeros(N,1);
end

%% Compute the regression by using the Iterative Landweber scheme with soft thresholding
while (Dfit>tollerance & cont < max_iter)
    
    cont = cont+1;
    b_temp = zeros(N,1);
    
    %% performs the Landweber iteration
    temp = (b(:,cont-1) - Z'*Z*b(:,cont-1) +Z'*y_std); 
    
    %% applies the soft thresholding at each iteration
    keep = (abs(temp)>thresh);                        %% Keeps the parameters larger than the threeshold (set the remaining to zero)    
   
    b_temp(keep,:) = temp(keep) - thresh*sign(temp(keep)); %% reduces the magnitude of the kept parameters  by threeh
    
    pred(:,cont) = (XX(end,:)*b_temp)*sy+my; %% Computes the forecasts
        
    fit = var(y_std-Z*b_temp); %% Computes the in-sample MSE
    
    Dfit = abs(fit-fit_prev); %% Compute the change in forecasts for checking convergence
    
    fit_prev = fit;
    
    b = [b b_temp];
    
end;

if cont == max_iter
    disp('LASSO: Maximum iteration reached')
end;