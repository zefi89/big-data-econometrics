########################################################################################################
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
#
########################################################################################################
#
# This file compares the forecating performances of alternative  forecasting methods for large cross-section
# in a simulated out-of-sample exercise using the Stock and Watson (2002)'s  macroeconomic panel
# of 131 monthly variables for the US economy for 1959 to 2002.
#
# The output of this program are statistics that compare the three methods.
# 1) Factor model forecasts (Principal components Regression)
# 2) Bayesian regression with i.i.d. normal prior (Ridge regression)
# 3) Bayesian regression with i.i.d. Laplatan prior (LASSO regression)
#
# In this file we use the three main following functions:
# PC_pred.m           Compute forecasts from Principal Components regression
# RIDGE_pred.m        Compute forecasts from RIDGE Regression
# LASSO_pred.m        Compute forecasts from LASSO Regression using the DDD thresholded Landweber iterative algorithm
# LARS_pred.m         Compute forecasts from LASSO Regression using the LARS algorithm
#
# See also: SET_LASSO.m, SET_RIDGE.m for the setting of the prior (penalization) parameters in LASSO and RIDGE regression
#           lars.m for the LARS algorithm (written by Karl Skoglund, IMM, DTU,)
# Remark: this file produces results for a single parameterization of the forecasting models.
#         Complete results for a grid of parameters (as in the paper) are produced by OutSample_complete.m
#########################################################################################################
message('----------------------------------------------------------------------------------------------------------------------------')

message('replication files:')
message(' ')
message('Forecasting using a large number of predictors:')
message('is Bayesian regression a valid alternative to principal components?')
message('Manuscript, ECARES-ULB, 2006 ')
message(' ')
message('Christine De Mol, Universite Libre de Bruxelles and ECARES,')
message('Domenico Giannone, Universite Libre de Bruxelles and ECARES')
message('Lucrezia Reichlin, European Central Bank, ECARES and CEPR')
message(' ')
message('Programs and manuscript available at:')
message('     http://homepages.ulb.ac.be/~dgiannon/ ')
message('     http://homepages.ulb.ac.be/~lreichli/ ')
message(' ')
message('Remark: this file produecs results for a single parameterization of the forecasting models.')
message('         Complete results for a grid of parameters (as in the paper) are produced by OutSample_complete.m')
message(' ')
message('See also: PC_pred.m, LASSO_pred.m, RIDGE_pred.m, SET_RIDGE.m, SET.LASSO.m, lars.m, OutSample_complete.m')
message('----------------------------------------------------------------------------------------------------------------------------')

#message(' ')
#message(' ')
#message('!!!WARNING!!!: the program computes forecasts for a range of parameters choice')
#message('               RUNNING THE WHOLEe OUT-OF-SAMPLE EXERCISE TAKES ABOUT 15 MINUTES')
#message('          ')
#message('               Answering N below you can use the forecasts for the parameterizations of the paper (saved in OutSample_compete.mat)')
#message('   ')
#reply <- readline("    Run the whole forceasting exercise ? Y/N [N]: ")
#message(' ')
#message(' ')

#stopifnot(reply == 'Y')

library(readxl)

source("Outliers.R")
#source("SetLasso.R")
#source("SetRidge.R")
source("GlmnetWrapper.R")

############
# Parameters
############

# Select the variables to predict
#   row 6 - "IPS10" : Industrial production (IP)
# ruw 114 - "PUNEW" : Consumer Price Index  (CPI)
nn <- c("IPS10", "PUNEW")

# Set the parameters
p     <-  0       # Number of lagged predictors
r     <-  10      # Number of principal components
K     <-  10      # Number of predictors to be selected by Lasso
INfit <- .5       # Proportion of in-sample fit to be explained by ridge

HH <- 12 # number of step ahead to forecast

# Number of time points to use in the estimation of the parameter: Rolling scheme
Jwind <- 120

start_date <- '1970-01-01'  # Starting dates for the out-of-sample evaluation

#####################################
# Reading in data and transforming it
#####################################

# Read in excel files
xlsfile <- "/home/jonas/Studium/2018_FS/big_data/project/BayPredRepFiles/hof.xls"
A <- read_excel(xlsfile, sheet = "Sheet1")
B <- read_excel(xlsfile, sheet = "Sheet3")

# Save relevant parts of excel sheet
transf <- A[2,2:NCOL(A)]    # Vector of indexes for transformations to apply to each series
data   <- A[3:(NROW(A)-2),] # First column is the date

# Make all columns in the data frame doubles
data[,colnames(data)]   <- lapply(data[,colnames(data)], as.numeric)

# Change type of first column to date
data$Date <- as.Date.numeric(data$Date, origin='1899-12-30')

## Repacle spaces with underscores in the series data frame
#series$Code = sub(" ", "_", series$Code)
#series$Transformation = t(transf[series$Code])

# Transpose data frame and add column label
transf <- data.frame(t(transf))
colnames(transf)[1] <- "Transformation"
#transf$Description <- series[rownames(transf)]

X <- data[14:NROW(data),]

#0: variable not to included;
# 1: no transformation
# 2: (1-L)
# 4: log * 100
# 5: (1-L) log * 100
# 6: (1-L) * (1-L^{12}) * log * 100 (Prices)

# Find the series names wich undergo the different transformation
for (ti in c(0,1,2,3,4,5,6)){
  labels <- rownames(subset(transf, Transformation == ti))

  # Drop the panel
  if (ti == 0) {
    X <- X[!(names(X) %in% labels)]
  }
  # (1-L)
  if (ti == 2) {
    X[labels] <- X[labels] - data[13:(NROW(data)-1),labels]
  }
  # log * 100
  if (ti == 4) {
    X[labels] <- log(X[labels]) * 100
  }
  # (1-L) log * 100
  #if (ti == 5) {
    #X[labels] <- 100 * (log(X[labels]) - log(data[13:(NROW(data)-1),labels]))
  #}
  # (1-L) * (1-L^{12}) * log * 100 (Prices)
  if (ti == 6) {
    X[labels] <- 100 * (log(X[labels]) - log(data[2:(NROW(data)-12),labels]))
               - 100 * (log(data[13:(NROW(data)-1),labels]) - log(data[1:(NROW(data)-13),labels]))
    data[labels] <- lapply(data[labels], as.numeric)
  }
}

################################################
# Getting subsamples for out-of-samples exercies
################################################

# Finds when to start the simulated out-of-sample exercise
start_sample <- which(X$Date == start_date)

#the rolling window cannot be larger than the first evaluation sample
stopifnot(Jwind <= start_sample)

j0 <- start_sample - Jwind + 1

x <- X[j0:start_sample,] # The available data at the beginning of the out of sample evaluation exercise

# Handle outliers for all panels except the two we want to predict
for (panel in setdiff(setdiff(names(x), nn), "Date")){
    x[panel] <- outliers(as.numeric(unlist(x[panel])))
}

############
# Regression
############

ridge.mod <- glmnetPred(x[nn[1]], x[,2:NCOL(x)], 0, 1, 1, 0)
print(ridge.mod)
