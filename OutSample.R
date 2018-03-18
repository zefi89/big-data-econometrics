library(readxl)

source("Outliers.R")
source("Predictors.R")

cv_plot_dir <- "plots_cv/"
dir.create(file.path(cv_plot_dir), showWarnings = FALSE)

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

# Scan variable for ridge
IN = c(0.1, 0.2, 0.3 ,0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
# Scan variable for PC and lasso
K = c(1, 2, 3, 5, 10, 25, 50, 60, 75, 100)

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

# 0: variable not to included
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
  if (ti == 5) {
    X[labels] <- 100 * (log(X[labels]) - log(data[13:(NROW(data)-1),labels]))
  }
  # (1-L) * (1-L^{12}) * log * 100 (Prices)
  if (ti == 6) {
    X[labels] <- 100 * (log(X[labels]) - log(data[2:(NROW(data)-12),labels])) -
                 100 * (log(data[13:(NROW(data)-1),labels]) - log(data[1:(NROW(data)-13),labels]))
  }
}

# The size of the panel
TT = NROW(X)
NN = NCOL(X) - 1


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

#######################
# Find tuning parameter
#######################

lambda_ridge = matrix(, nrow = length(IN), ncol = 3)
lambda_ridge[,1] = IN
colnames(lambda_ridge) <- c("IN", nn[1], nn[2])
lambda_ridge_best <- c()

lambda_lasso = matrix(, nrow = length(K), ncol = 3)
lambda_lasso[,1] = K
colnames(lambda_lasso) <- c("K", nn[1], nn[2])
lambda_lasso_best <- c()

for (k in 1:length(nn)) {

    # Ridge
    for (i in 1:length(IN)) {
        fit <- setRidge(x[nn[k]], x[,2:NCOL(x)], p=0, INfit=IN[i], h=HH) # Ridge
        lambda_ridge[i,k+1] = fit$lambda
    }
    # with cross validation
    fit <- glmnetPred(x[nn[k]], x[,2:NCOL(x)], p=0, h=HH, alpha=0, cv=TRUE, cv_plot_file=paste(cv_plot_dir,paste(nn[k], '_ridge_cv.pdf', sep=""), sep=""))
    lambda_ridge_best[k] = fit$lambda

    # Lasso
    for (i in 1:length(K)) {
        fit <- setLasso(x[nn[k]], x[,2:NCOL(x)], p=0, K=K[i], h=HH) # Lasso
        lambda_lasso[i,k+1] = fit$lambda
    }
    # with cross validation
    fit <- glmnetPred(x[nn[k]], x[,2:NCOL(x)], p=0, h=HH, alpha=1, cv=TRUE, cv_plot_file=paste(cv_plot_dir,paste(nn[k], '_lasso_cv.pdf', sep=""), sep=""))
    lambda_lasso_best[k] = fit$lambda

    # Regression tree (best pruning parameter is found for every prediction anyway, this is only for the cv plots)
    rpartPred(x[nn[k]], x[,2:NCOL(x)], p=0, h=HH, cvplot=TRUE, cv_plot_file=paste(cv_plot_dir,paste(nn[k], '_tree_cv.pdf', sep=""), sep=""))
    rpartPred(x[nn[k]], x[,2:NCOL(x)], p=0, h=HH, cvplot=TRUE, cv_plot_file=paste(cv_plot_dir,paste(nn[k], '_tree_cv.pdf', sep=""), sep=""))
}


#################################################
# Performs the out-of-sample forecasting exercise
#################################################

months <- as.numeric(format(as.Date(X$Date), "%m"))

ridge = matrix(, nrow = TT, ncol = 2 * (length(IN) + 1))
colnames(ridge) <- c(
                     paste(nn[1], "_ridge_IN0p1", sep=""), paste(nn[2], "_ridge_IN0p1", sep=""),
                     paste(nn[1], "_ridge_IN0p2", sep=""), paste(nn[2], "_ridge_IN0p2", sep=""),
                     paste(nn[1], "_ridge_IN0p3", sep=""), paste(nn[2], "_ridge_IN0p3", sep=""),
                     paste(nn[1], "_ridge_IN0p4", sep=""), paste(nn[2], "_ridge_IN0p4", sep=""),
                     paste(nn[1], "_ridge_IN0p5", sep=""), paste(nn[2], "_ridge_IN0p5", sep=""),
                     paste(nn[1], "_ridge_IN0p6", sep=""), paste(nn[2], "_ridge_IN0p6", sep=""),
                     paste(nn[1], "_ridge_IN0p7", sep=""), paste(nn[2], "_ridge_IN0p7", sep=""),
                     paste(nn[1], "_ridge_IN0p8", sep=""), paste(nn[2], "_ridge_IN0p8", sep=""),
                     paste(nn[1], "_ridge_IN0p9", sep=""), paste(nn[2], "_ridge_IN0p9", sep=""),
                     paste(nn[1], "_ridge_cv", sep=""), paste(nn[2], "_ridge_cv", sep="")
                     )

lasso = matrix(, nrow = TT, ncol = 2 * (length(K) + 1))
colnames(lasso) <- c(
                     paste(nn[1], "_lasso_K1", sep=""), paste(nn[2], "_lasso_K1", sep=""),
                     paste(nn[1], "_lasso_K2", sep=""), paste(nn[2], "_lasso_K2", sep=""),
                     paste(nn[1], "_lasso_K3", sep=""), paste(nn[2], "_lasso_K3", sep=""),
                     paste(nn[1], "_lasso_K5", sep=""), paste(nn[2], "_lasso_K5", sep=""),
                     paste(nn[1], "_lasso_K10", sep=""), paste(nn[2], "_lasso_K10", sep=""),
                     paste(nn[1], "_lasso_K25", sep=""), paste(nn[2], "_lasso_K25", sep=""),
                     paste(nn[1], "_lasso_K50", sep=""), paste(nn[2], "_lasso_K50", sep=""),
                     paste(nn[1], "_lasso_K60", sep=""), paste(nn[2], "_lasso_K60", sep=""),
                     paste(nn[1], "_lasso_K75", sep=""), paste(nn[2], "_lasso_K75", sep=""),
                     paste(nn[1], "_lasso_K100", sep=""), paste(nn[2], "_lasso_K100", sep=""),
                     paste(nn[1], "_lasso_cv", sep=""), paste(nn[2], "_lasso_cv", sep="")
                     )

pc = matrix(, nrow = TT, ncol = 2 * length(K))
colnames(pc) <- c(
                     paste(nn[1], "_pc_r1", sep=""), paste(nn[2], "_pc_r1", sep=""),
                     paste(nn[1], "_pc_r2", sep=""), paste(nn[2], "_pc_r2", sep=""),
                     paste(nn[1], "_pc_r3", sep=""), paste(nn[2], "_pc_r3", sep=""),
                     paste(nn[1], "_pc_r5", sep=""), paste(nn[2], "_pc_r5", sep=""),
                     paste(nn[1], "_pc_r10", sep=""), paste(nn[2], "_pc_r10", sep=""),
                     paste(nn[1], "_pc_r25", sep=""), paste(nn[2], "_pc_r25", sep=""),
                     paste(nn[1], "_pc_r50", sep=""), paste(nn[2], "_pc_r50", sep=""),
                     paste(nn[1], "_pc_r60", sep=""), paste(nn[2], "_pc_r60", sep=""),
                     paste(nn[1], "_pc_r75", sep=""), paste(nn[2], "_pc_r75", sep=""),
                     paste(nn[1], "_pc_r100", sep=""), paste(nn[2], "_pc_r100", sep="")
                     )

naive = matrix(, nrow = TT, ncol = 2)
colnames(naive) <- c(paste(nn[1], "_rw", sep=""), paste(nn[2], "_rw", sep=""))
true = matrix(, nrow = TT, ncol = 2)
colnames(true) <- c(paste(nn[1], "_true", sep=""), paste(nn[2], "_true", sep=""))
tree = matrix(, nrow = TT, ncol = 2)
colnames(tree) <- c(paste(nn[1], "_tree", sep=""), paste(nn[2], "_tree", sep=""))
forest = matrix(, nrow = TT, ncol = 2)
colnames(forest) <- c(paste(nn[1], "_forest", sep=""), paste(nn[2], "_forest", sep=""))

for (j in start_sample:(TT-HH)) {
#for (j in start_sample:(start_sample + 10)) {

    # Displays the dates at the beginning of each year
    if (months[j] == 1) {
        message('--------------')
        message('now running')
        message(X$Date[j])
    }

    # Define the beginning of the estimation sample

    j0 <- j - Jwind + 1

    x <- X[j0:j,] # The available data at each time point of the evaluation exercise

    # Handle outliers for all panels except the two we want to predict
    for (panel in setdiff(setdiff(names(x), nn), "Date")){
        x[panel] <- outliers(as.numeric(unlist(x[panel])))
    }

    # Normalization constant
    const = 12

    for (k in 1:length(nn)) {
        # Computed the ridge forecasts
        for (i in 1:length(IN)) {
            if (i == length(IN)) {
                fit <- glmnetPred(x[nn[k]], x[,2:NCOL(x)], p=0, cv=TRUE, h=HH, alpha=1)
            } else {
                fit <- glmnetPred(x[nn[k]], x[,2:NCOL(x)], p=0, lambda=lambda_ridge[i,k+1], h=HH, alpha=0)
            }
            ridge[j+HH,k+2*(i-1)] = fit$pred * const
        }
        # with the best lambda from cross validation
        fit <- glmnetPred(x[nn[k]], x[,2:NCOL(x)], p=0, lambda=lambda_ridge_best[k], h=HH, alpha=0)
        ridge[j+HH,k+2*length(IN)] = fit$pred * const

        # The lasso forecasts
        for (i in 1:length(K)) {
            if (i == length(K)) {
                fit <- glmnetPred(x[nn[k]], x[,2:NCOL(x)], p=0, cv=TRUE, h=HH, alpha=1)
            } else {
                fit <- glmnetPred(x[nn[k]], x[,2:NCOL(x)], p=0, lambda=lambda_lasso[i,k+1], h=HH, alpha=1)
            }
            lasso[j+HH,k+2*(i-1)] = fit$pred * const
        }
        # with the best lambda from cross validation
        fit <- glmnetPred(x[nn[k]], x[,2:NCOL(x)], p=0, lambda=lambda_lasso_best[k], h=HH, alpha=1)
        lasso[j+HH,k+2*length(K)] = fit$pred * const

        # The PC forecasts (the r in PC are going over the same values as the K in lasso)
        for (i in 1:length(K)) {
            pred <- pcPred(x[nn[k]], x[,2:NCOL(x)], p=0, r=K[i], h=HH)
            pc[j+HH,k+2*(i-1)] = pred * const
        }

        # Regression tree forecast
        tree[j+HH,k] = rpartPred(x[nn[k]], x[,2:NCOL(x)], p=0, h=HH) * const

        # Random forest forecast
        forest[j+HH,k] = rpartPred(x[nn[k]], x[,2:NCOL(x)], p=0, h=HH, random_forest=TRUE) * const

        # The random walk forecast
        naive[j+HH,k] = rwPred(x[nn[k]], h=HH) * const

        # Compute the true value to be predicted
        temp = as.numeric(colMeans(X[(j+1):(j+HH),nn[k]]))
        true[j+HH,k] = temp*const
    }

}

#######################################
# Save true values and forecasts to CSV
#######################################

data <- cbind(cbind(cbind(cbind(cbind(cbind(true, naive), ridge), lasso), pc), tree), forest)[(start_sample+HH):NROW(X),]
dates <- X[(start_sample+HH):NROW(X),1]

datawithdates <- cbind(dates, data)
datawithdates$Date <- as.numeric(datawithdates$Date)

save(data, dates, lambda_ridge, lambda_lasso, lambda_lasso_best, lambda_ridge_best, file="OutSample.Rda")
write.csv(datawithdates, "OutSample.csv", row.names=FALSE)
