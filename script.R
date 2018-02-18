library(readxl)

# Read in excel files
xlsfile <- "/home/jonas/Studium/2018_FS/big_data/project/BayPredRepFiles/hof.xls"
A <- read_excel(xlsfile, sheet = "Sheet1")
B <- read_excel(xlsfile, sheet = "Sheet3")

# Save relevant parts of excel sheet
transf <- A[2,2:NCOL(A)]    # Vector of indexes for transformations to apply to each series
data   <- A[3:(NROW(A)-2),] # First column is the date
data[,colnames(data)]   <- lapply(data[,colnames(data)], as.numeric)
#series <- B[1:NROW(B),2:3]  # Labels of the variables in the panel

# Change type of first column to date
data$Date <- as.Date.numeric(data$Date, origin='1899-12-30')

## Repacle spaces with underscores in the series data frame
#series$Code = sub(" ", "_", series$Code)
#series$Transformation = t(transf[series$Code])

transf <- data.frame(t(transf))
colnames(transf)[1] <- "Transformation"
#transf$Description <- series[rownames(transf)]

X <- data[14:NROW(data),]

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
    X[labels] <- 100 * (log(X[labels]) - log(data[2:(NROW(data)-12),labels])) - 100 * (log(data[13:(NROW(data)-1),labels]) - log(data[1:(NROW(data)-13),labels]))
    data[labels] <- lapply(data[labels], as.numeric)
  }
}

#0: variable not to included;
# 1: no transformation
# 2: (1-L)
# 4: log * 100
# 5: (1-L) log * 100
# 6: (1-L) * (1-L^{12}) * log * 100 (Prices)

# Transforms the data
#if transf = 0, remove the variable from the panel
#print(data)
#data <- data[:,transf!=0]
#series = series(transf~=0)
#transf = transf(transf~=0)
