 Replications files for the paper:

 Forecasting using a large number of predictors: 
 is Bayesian regression a valid alternative to principal components?
 Manuscript, ECARES-ULB, 2006 
 
 Christine De Mol, Universite' Libre de Bruxelles and ECARES,
 Domenico Giannone, Universite' Libre de Bruxelles and ECARES,
 Lucrezia Reichlin, European Central Bank, ECARES and CEPR

 Programs and manuscript available at: 
 http://homepages.ulb.ac.be/~dgiannon/
 http://homepages.ulb.ac.be/~lreichli/

 The main programs are OutSample.m and OutSample_complete.m
 These files compare the forecating performances of alternative  forecasting methods for large cross-section 
 in a simulated out-of-sample exercise using the Stock and Watson (2002)'s  macroeconomic panel 
 of 131 monthly variables for the US economy for 1959 to 2002.

 The output of this program are statistics that compare the three methods.
 1) Factor model forecasts (Principal components Regression)                        
 2) Bayesian regression with i.i.d. normal prior (Ridge regression) 
 3) Bayesian regression with i.i.d. Laplatan prior (LASSO regression)

 OutSample.m this file produces results for a single parameterization of the forecasting models.
         
 Complete results for a grid of parameters (as in the paper) are produced by OutSample_complete.m 

 
 The forecasts are produced from the three main following functions:
 PC_pred.m           Compute forecasts from Principal Components regression
 RIDGE_pred.m        Compute forecasts from RIDGE Regression  
 LASSO_pred.m        Compute forecasts from LASSO Regression 

 See also: SET_LASSO.m, SET_RIDGE.m for the setting of the prior (penalization) parameters in LASSO and RIDGE regression
           lars.m, used in LARS_pred, is by Karl Skoglund, IMM, DTU