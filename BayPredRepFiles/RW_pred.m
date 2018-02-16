function pred = ADL_fact(y,h);
% Compute h-step ahead prediction from constant growth model
% Y = c+e
% Y = (y_{+1}+...+y_{+h})/h; %
% Input:
% y:       dependent variable
% h:       number of steps ahead

% Output:
% pred:    h-step ahead predictions (\hat y_{T+h}+...+y_{T+1})/h 

%% Compute the variable to be predicted
%% Y = (y_{+1}+...+y_{+h})/h
Y = filter(ones(h,1)/h,1,y);
Y = Y(h+1:end,:);

%% Compute the prediction
pred = mean(Y);