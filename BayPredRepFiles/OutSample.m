%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This file compares the forecating performances of alternative  forecasting methods for large cross-section 
% in a simulated out-of-sample exercise using the Stock and Watson (2002)'s  macroeconomic panel 
% of 131 monthly variables for the US economy for 1959 to 2002.
%
% The output of this program are statistics that compare the three methods.
% 1) Factor model forecasts (Principal components Regression)                        
% 2) Bayesian regression with i.i.d. normal prior (Ridge regression) 
% 3) Bayesian regression with i.i.d. Laplatan prior (LASSO regression)
% 
% In this file we use the three main following functions:
% PC_pred.m           Compute forecasts from Principal Components regression
% RIDGE_pred.m        Compute forecasts from RIDGE Regression  
% LASSO_pred.m        Compute forecasts from LASSO Regression using the DDD thresholded Landweber iterative algorithm 
% LARS_pred.m         Compute forecasts from LASSO Regression using the LARS algorithm
%
% See also: SET_LASSO.m, SET_RIDGE.m for the setting of the prior (penalization) parameters in LASSO and RIDGE regression
%           lars.m for the LARS algorithm (written by Karl Skoglund, IMM, DTU,)
% Remark: this file produces results for a single parameterization of the forecasting models.
%         Complete results for a grid of parameters (as in the paper) are produced by OutSample_complete.m 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
disp('----------------------------------------------------------------------------------------------------------------------------')

disp('replication files:')
disp(' ')
disp('Forecasting using a large number of predictors:')
disp('is Bayesian regression a valid alternative to principal components?')
disp('Manuscript, ECARES-ULB, 2006 ')
disp(' ')
disp('Christine De Mol, Universite Libre de Bruxelles and ECARES,')
disp('Domenico Giannone, Universite Libre de Bruxelles and ECARES')
disp('Lucrezia Reichlin, European Central Bank, ECARES and CEPR')
disp(' ')
disp('Programs and manuscript available at:')
disp('     http://homepages.ulb.ac.be/~dgiannon/ ')
disp('     http://homepages.ulb.ac.be/~lreichli/ ')
disp(' ')
disp('Remark: this file produecs results for a single parameterization of the forecasting models.')
disp('         Complete results for a grid of parameters (as in the paper) are produced by OutSample_complete.m')
disp(' ')
disp('See also: PC_pred.m, LASSO_pred.m, RIDGE_pred.m, SET_RIDGE.m, SET.LASSO.m, lars.m, OutSample_complete.m')
disp('----------------------------------------------------------------------------------------------------------------------------')


clear;
disp(' ')
disp(' ')
disp('!!!WARNING!!!: the program computes forecasts for a range of parameters choice')
disp('               RUNNING THE WHOLEe OUT-OF-SAMPLE EXERCISE TAKES ABOUT 15 MINUTES')
disp('          ')
disp('               Answering N below you can use the forecasts for the parameterizations of the paper (saved in OutSample_compete.mat)')
disp('   ')
reply = input('    Run the whole forceasting exercise ? Y/N [N]: ','s');
disp(' ')
disp(' ')
if isempty(reply)
    reply = 'N';
end;

if reply == 'Y'    
    
    %% Select the variables to predict
    %% 6   : Industrial production (IP)
    %% 114 : Consumer Price Index  (CPI)
    nn = [6 114];
    
    %% Set the parameters
    p     =  0;        %% Number of lagged predictors
    r     =  10;        %% Number of principal components
    K     =  10;       %% Number of predictors to be selected by Lasso
    INfit = .5;       %% Proportion of in-sample fit to be explained by ridge
    
    HH = [12]; %% number of step ahead to forecast
    
    %% Number of time points to use in the estimation of the parameter: Rolling scheme 
    Jwind = 120; 
    
    start_y = 1970; start_m = 1; %% Starting dates for the out-of-sample evaluation
    
    
    %% Loads the data from the Stock and Watson (2005) database
    [A,B] = xlsread('hof.xls'); 
    matlabDates = datenum('30-Dec-1899') + A(2:end,1);
    dates = matlabDates(1:end,1);
    time = datevec(dates);
    
    DATA = A(2:end,2:end); %% The data matrix: time in rows, variables in colums
    series = B(1,2:end);   %% Labels of the variables in the panel
    
    transf = A(1,2:end);   %% Vector of indexes for transformations to apply to each series
    %% 0: variable not to included;
    %% 1: no transformation
    %% 2: (1-L)
    %% 4: log * 100
    %% 5: (1-L) log * 100
    %% 6: (1-L) * (1-L^{12}) * log * 100 (Prices)
    
    
    %% Transforms the data
    %% if transf = 0, remove the variable from the panel
    DATA = DATA(:,transf~=0);
    series = series(transf~=0);
    transf = transf(transf~=0);
    
    for j = 1:length(transf);
        tr = transf(j);
        if tr == 1
            X(:,j) = DATA(14:end,j);%% no transformation
            
        elseif tr == 2              %% (1-L)
            X(:,j) = DATA(14:end,j) - DATA(13:end-1,j);
            
        elseif tr == 4              %% log * 100
            X(:,j) = log(DATA(14:end,j))*100;
            
        elseif tr == 5              %% (1-L) log * 100
            X(:,j) = (log(DATA(14:end,j)) - log(DATA(13:end-1,j)))*100;
            
        elseif tr == 6              %% (1-L) * (1-L^{12}) * log * 100 (Prices)
            X(:,j) = (log(DATA(14:end,j)) - log(DATA(2:end-12,j)))*100-(log(DATA(13:end-1,j)) - log(DATA(1:end-13,j)))*100;
            
        end;
    end;
    
    %% Reset time and dates to take since initial points are removed when
    %% performing the ransformations
    time = time(14:end,:);
    dates = dates(14:end,:);
    
    %% The size of the panel
    [TT,NN] = size(X);
    
    tic 
    
    
    % Finds when to start the simulated out-of-sample exercise
    start_sample = find((time(:,1)==start_y) & (time(:,2)==start_m));
    if Jwind > start_sample
        error('the rolling window cannot be larger than the first evaluation sample')
    end;
    
    
    
    j0=start_sample-Jwind+1;
    
    x_temp = X(j0:start_sample,:);%% The available data at the beginning of the out of sample evaluation exercise
    clear x
    for jn = 1:size(x_temp,2)
        x(:,jn) = outliers(x_temp(:,jn));
    end;
    
    x(:,nn) = x_temp(:,nn);
    
    %% Set the LASSO penalization parameter so as to keep K predictors at the
    %% beginning of the simulated out-of-sample exercise (the parameter is used for the DDD algorithm)
    for k = 1:length(nn) %% Loop across series to forecast
        for h = HH  %% Loop across forecast horizons
            nu_lasso{h}(k) = SET_lasso(x(:,nn(k)),x,p,K,h);
        end;
    end;
    
    
    %% Set the RIDGE penalization parameter to explain INfit proportion of variance at the
    %% beginning of the simulated out-of-sample exercise
    for k = 1:length(nn) %% Loop across series to forecast
        for h = HH  %% Loop across forecast horizons
            nu_ridge{h}(k) = SET_ridge(x(:,nn(k)),x,p,INfit,h);
        end;
    end;
    
    
    
    
    %% Performs the out-of-sample forecasting exercise
    for j = start_sample:TT-HH(end)
        
        %% Displays the dates at the beginning of each year
        if time(j,2)==1
            disp('--------------')
            disp('now running')
            disp(time(j,1:2))
            disp(['elapsed minutes:  ',num2str(toc/60)])
        end;
        
        %% Define the beginning of the estimation sample
        
        j0=j-Jwind+1;
        
        x_temp = X(j0:j,:);%% The available data at each time point of the evaluation exercise
        clear x
        for jn = 1:size(x_temp,2)
            x(:,jn) = outliers(x_temp(:,jn));
        end;
        
        x(:,nn) = x_temp(:,nn);
        
        
        for k = 1:length(nn) %% Loop across series to forecast
            
            
            for h = HH  %% Loop across number of steps ahead
                
                
                %% Normalization constants;
                if sum(nn(k) == [1 6 25])
                    const = 12;
                else 
                    const = h;
                end;
                
                %% Computed the LASSO forecasts using the threeshold Landweber algorithm
                if j==start_sample
                    [pred,b] = LASSO_pred(x(:,nn(k)),x,p,nu_lasso{h}(k),h);
                else
                    [pred,b] = LASSO_pred(x(:,nn(k)),x,p,nu_lasso{h}(k),h,BETA{h}(:,j-1,k));
                end;
                LASSO{h}(j+h,k) = pred(end)*const;
                BETA{h}(:,j,k)   = b(:,end); %% Save the estimated regresion coefficients 
                niter_lasso{h}(j,k) = length(pred);
                
                
                %% Computed the sequence of LASSO forecasts using the LARS
                %% algorithm, one for each umber of selected predictors
                [pred,b] = LARS_pred(x(:,nn(k)),x,p,h);
                
                LARS{h}(j+h,k) = pred(end,K+1)*const; % Select the prediction based on K predictors 
                BETA_lars{h}(:,j,k)   = b(K+1,:)'; %% Save the estimated regresion coefficients associatied with the selection of K predictors
                
                %% Computed the ridge forecasts
                [pred,b,MSE] = RIDGE_pred(x(:,nn(k)),x,p,nu_ridge{h}(k),h);
                RIDGE{h}(j+h,k) = pred(end)*const;
                INFIT{h}(j,k) = MSE; %% Save the explained variance in-sample
                
                %% Compute the true value to be predicted
                temp = mean(X(j+1:j+h,nn(k)));
                true{h}(j+h,k) = temp*const;
                
                %% Compute the Constant growth forecast
                temp = RW_pred(x(:,nn(k)),h);
                RW{h}(j+h,k) = temp*const;
                
                %% Computes the Factor based forecasts
                temp = PC_pred(x(:,nn(k)),x,p,r,h);
                PC{h}(j+h,k) = temp*const;
                
                
                
                
                
            end;
        end;
    end;
    
    save OutSample
    
else
    
    load OutSample 
    
    %% Load the forecasts computed for the following specifications  
    %% Variables predicted
    %% nn = [6 114];
    %% 6   : Industrial production (IP)
    %% 114 : Consumer Price Index  (CPI)
    
    %% Parameters
    %% p     =  0;                       %% Number of lagged predictors
    %% rr    =  10                       %% Number of principal components
    %% K     =  10                       %% Number of predictors to be selected by Lasso
    %% INfit = .5                        %% Proportion of in-sample fit to be explained by ridge
    %% HH    = [12];                     %% number of step ahead to forecast
    %% Number of time points to use in the estimation of the parameter: Rolling scheme 
    %% Jwind = 120;                      %% Number of observations used for estimation (rolling forecasting scheme)
    %% start_y = 1970; start_m = 1;      %% Starting dates for the out-of-sample evaluation
    
end;
%% Evaluate the prediction accuracy for the whole sample 1970-2002

first_y = 1970; first_m = 12; %% Dates of the beginning of the evaluation sample
ind_first = find((time(:,1)==first_y) & (time(:,2)==first_m));

last_y = 2002; last_m = 12;   %% Dates of the end of the evaluation sample
ind_last = find((time(:,1)==last_y) & (time(:,2)==last_m));


%% Compute the MSFE
for h = HH
    for k = 1:length(nn)
        MSFE_RW(h,k)    = mean((true{h}(ind_first:ind_last,k)-RW{h}(ind_first:ind_last,k)).^2);
        MSFE_PC(h,k)    =  mean((true{h}(ind_first:ind_last,k)-PC{h}(ind_first:ind_last,k)).^2);
        MSFE_LARS(h,k) =  mean((true{h}(ind_first:ind_last,k)-LARS{h}(ind_first:ind_last,k)).^2);
        MSFE_RIDGE(h,k) =  mean((true{h}(ind_first:ind_last,k)-RIDGE{h}(ind_first:ind_last,k)).^2);
        MSFE_LASSO(h,k) =  mean((true{h}(ind_first:ind_last,k)-LASSO{h}(ind_first:ind_last,k)).^2);
    end    
end;



%% Print the results

for k = 1:length(nn)
    disp('------------------------------------------------------------------------')
    disp('------------------------------------------------------------------------')
    
    disp(['evaluation sample:  from ',num2str([first_y first_m]),'      to         ',num2str([last_y last_m])])
    disp('------------------------------------------------------------------------')
    
    disp(['foecasting     ',series{nn(k)}] )
    disp('------------------------------------------------------------------------')
    disp('Parameterization:')
    disp(['PC   : r = ', num2str(r),' principal components'])
    disp(' ')
    disp('RIDGE '),
    disp(['nu = ', num2str(nu_ridge{h}(k)),' penalization parameter selected at the beginning of the forecasting sample '])
    disp(['to explain ',num2str(INfit*100), '% of the in-sample variance (at the beginning of the forecasting exercise)']) 
    disp(' ')
    disp('LASSO with LARS, keeps the same number of selected predictors for the each step of the out-of-sample:'),
    disp(['Select ', num2str(K), ' predictors at each step']) 
    disp(' ')
    disp('LASSO with iterated Landweber, keeps the same penalzation parameter for the each step of the out-of-sample:'),
    disp(['nu = ', num2str(nu_lasso{h}(k)),' penalization parameter selected at the beginning of the forecasting sample'])
    disp(['to keep ', num2str(K), ' predictors at the beginning of the forecasting exercise']) 
    
    
    
    disp('------------------------------------------------------------------------')
    
    disp('Mean Squared Forecast Errors')
    
    disp(' horizon       RW       PC/RW    LASSO/RW   LARS/RW  RIDGE/RW')
    
    disp([HH'  MSFE_RW(HH,k) MSFE_PC(HH,k)/MSFE_RW(HH,k) MSFE_LASSO(HH,k)/MSFE_RW(HH,k) MSFE_LARS(HH,k)/MSFE_RW(HH,k) MSFE_RIDGE(HH,k)/MSFE_RW(HH,k)])
    disp('------------------------------------------------------------------------')
    
end;



%% Plot the forecasts
cont = 0;
for h = HH;
    for k = 1:length(nn)
        cont = cont+1;
        figure(cont)
        plot(dates(ind_first:ind_last),[true{h}(ind_first:ind_last,k) RW{h}(ind_first:ind_last,k) RIDGE{h}(ind_first:ind_last,k)...
                PC{h}(ind_first:ind_last,k) LASSO{h}(ind_first:ind_last,k) LARS{h}(ind_first:ind_last,k)])
        datetick('x','mmmyyyy');title(['forecasting ', series{nn(k)},' ',num2str(h),'-steps ahead'])
        legend('True','RW','RIDGE','PC','LASSO','LARS');
    end
end;

%% Evaluate the prediction accuracy for the I subsample: 1970-1984

first_y = 1970; first_m = 12; %% Dates of the beginning of the evaluation sample
ind_first = find((time(:,1)==first_y) & (time(:,2)==first_m));

last_y = 1984; last_m = 12;   %% Dates of the end of the evaluation sample
ind_last = find((time(:,1)==last_y) & (time(:,2)==last_m));



%% Compute the MSFE
for h = HH
    for k = 1:length(nn)
        MSFE_RW(h,k)    = mean((true{h}(ind_first:ind_last,k)-RW{h}(ind_first:ind_last,k)).^2);
        MSFE_PC(h,k)    =  mean((true{h}(ind_first:ind_last,k)-PC{h}(ind_first:ind_last,k)).^2);
        MSFE_LARS(h,k) =  mean((true{h}(ind_first:ind_last,k)-LARS{h}(ind_first:ind_last,k)).^2);
        MSFE_RIDGE(h,k) =  mean((true{h}(ind_first:ind_last,k)-RIDGE{h}(ind_first:ind_last,k)).^2);
        MSFE_LASSO(h,k) =  mean((true{h}(ind_first:ind_last,k)-LASSO{h}(ind_first:ind_last,k)).^2);
    end    
end;



%% Print the results

for k = 1:length(nn)
    disp('------------------------------------------------------------------------')
    disp('------------------------------------------------------------------------')
    
    disp(['evaluation sample:  from ',num2str([first_y first_m]),'      to         ',num2str([last_y last_m])])
    disp('------------------------------------------------------------------------')
    
    disp(['foecasting     ',series{nn(k)}] )
    disp('------------------------------------------------------------------------')
    disp('Parameterization:')
    disp(['PC   : r = ', num2str(r),' principal components'])
    disp(' ')
    disp('RIDGE '),
    disp(['nu = ', num2str(nu_ridge{h}(k)),' penalization parameter selected at the beginning of the forecasting sample '])
    disp(['to explain ',num2str(INfit*100), '% of the in-sample variance (at the beginning of the forecasting exercise)']) 
    disp(' ')
    disp('LASSO with LARS, keeps the same number of selected predictors for the each step of the out-of-sample:'),
    disp(['Select ', num2str(K), ' predictors at each step']) 
    disp(' ')
    disp('LASSO with iterated Landweber, keeps the same penalzation parameter for the each step of the out-of-sample:'),
    disp(['nu = ', num2str(nu_lasso{h}(k)),' penalization parameter selected at the beginning of the forecasting sample'])
    disp(['to keep ', num2str(K), ' predictors at the beginning of the forecasting exercise']) 
    
    
    
    disp('------------------------------------------------------------------------')
    
    disp('Mean Squared Forecast Errors')
    
    disp(' horizon       RW       PC/RW    LASSO/RW   LARS/RW  RIDGE/RW')
    
    disp([HH'  MSFE_RW(HH,k) MSFE_PC(HH,k)/MSFE_RW(HH,k) MSFE_LASSO(HH,k)/MSFE_RW(HH,k) MSFE_LARS(HH,k)/MSFE_RW(HH,k) MSFE_RIDGE(HH,k)/MSFE_RW(HH,k)])
    disp('------------------------------------------------------------------------')
    
end;


%% Evaluate the prediction accuracy for the II subsample: 1985-2002

first_y = 1985; first_m = 1; %% Dates of the beginning of the evaluation sample
ind_first = find((time(:,1)==first_y) & (time(:,2)==first_m));

last_y = 2002; last_m = 12;   %% Dates of the end of the evaluation sample
ind_last = find((time(:,1)==last_y) & (time(:,2)==last_m));


%% Compute the MSFE
for h = HH
    for k = 1:length(nn)
        MSFE_RW(h,k)    = mean((true{h}(ind_first:ind_last,k)-RW{h}(ind_first:ind_last,k)).^2);
        MSFE_PC(h,k)    =  mean((true{h}(ind_first:ind_last,k)-PC{h}(ind_first:ind_last,k)).^2);
        MSFE_LARS(h,k) =  mean((true{h}(ind_first:ind_last,k)-LARS{h}(ind_first:ind_last,k)).^2);
        MSFE_RIDGE(h,k) =  mean((true{h}(ind_first:ind_last,k)-RIDGE{h}(ind_first:ind_last,k)).^2);
        MSFE_LASSO(h,k) =  mean((true{h}(ind_first:ind_last,k)-LASSO{h}(ind_first:ind_last,k)).^2);
    end    
end;



%% Print the results

for k = 1:length(nn)
    disp('------------------------------------------------------------------------')
    disp('------------------------------------------------------------------------')
    
    disp(['evaluation sample:  from ',num2str([first_y first_m]),'      to         ',num2str([last_y last_m])])
    disp('------------------------------------------------------------------------')
    
    disp(['foecasting     ',series{nn(k)}] )
    disp('------------------------------------------------------------------------')
    disp('Parameterization:')
    disp(['PC   : r = ', num2str(r),' principal components'])
    disp(' ')
    disp('RIDGE '),
    disp(['nu = ', num2str(nu_ridge{h}(k)),' penalization parameter selected at the beginning of the forecasting sample '])
    disp(['to explain ',num2str(INfit*100), '% of the in-sample variance (at the beginning of the forecasting exercise)']) 
    disp(' ')
    disp('LASSO with LARS, keeps the same number of selected predictors for the each step of the out-of-sample:'),
    disp(['Select ', num2str(K), ' predictors at each step']) 
    disp(' ')
    disp('LASSO with iterated Landweber, keeps the same penalzation parameter for the each step of the out-of-sample:'),
    disp(['nu = ', num2str(nu_lasso{h}(k)),' penalization parameter selected at the beginning of the forecasting sample'])
    disp(['to keep ', num2str(K), ' predictors at the beginning of the forecasting exercise']) 
    
    
    
    disp('------------------------------------------------------------------------')
    
    disp('Mean Squared Forecast Errors')
    
    disp(' horizon       RW       PC/RW    LASSO/RW   LARS/RW  RIDGE/RW')
    
    disp([HH'  MSFE_RW(HH,k) MSFE_PC(HH,k)/MSFE_RW(HH,k) MSFE_LASSO(HH,k)/MSFE_RW(HH,k) MSFE_LARS(HH,k)/MSFE_RW(HH,k) MSFE_RIDGE(HH,k)/MSFE_RW(HH,k)])
    disp('------------------------------------------------------------------------')
    
end;