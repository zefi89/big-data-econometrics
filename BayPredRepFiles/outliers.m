function [Z,Jout] = outliers(X);
% function [Z,Jout] = outliers(X);
% adjust for outliers, 
% observations with absolute value larger then 6 times the
% interquartile distance are replaced with their median

T = size(X,1);
a = sort(X);

%%% define outliers as those obs. that exceed 6 times the interquartile
%%% distance

Jout = (abs(X-a(round(T/2))) > 10*abs(a(round(T*3/4))-a(round(T*1/4))));

Z = X;
Z(Jout) = a(round(T/2)); %% put the median in place of outliers


%% Replace outliers with centered moving average of order 3. 
Zma = MAcentered(Z,3); 

Z(Jout) = Zma(Jout);

%% this function compute MA of order k
function x_ma = MAcentered(x,k_ma);
xpad = [x(1,:).*ones(k_ma,1); x; x(end,:).*ones(k_ma,1)];
for j = k_ma+1:length(xpad)-k_ma;
    x_ma(j-k_ma,:) = mean(xpad(j-k_ma:j+k_ma,:));
end;
