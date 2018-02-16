library(readxl)

xlsfile <- "Studium/2018_FS/big_data/project/hof.xls"
A <- read_excel(xlsfile, sheet = "Sheet1")
B <- read_excel(xlsfile, sheet = "Sheet3")

transf <- A[2,2:NCOL(A)]         # Vector of indexes for transformations to apply to each series
data <- A[3:(NROW(A)-2),]          # First column is the date


data$Date <- as.Date.numeric(data$Date, origin='1899-12-30')

# Do the transformations
data[,2:NCOL(data)][,which(transf != 0)] # Drop panels where transf == 0

series <- B[1:NROW(B),2:3] # Labels of the variables in the panel
series$Code = sub(" ", "_", series$Code)
series$Transformation = t(transf[series$Code])


print(transf)
#0: variable not to included;
# 1: no transformation
# 2: (1-L)
# 4: log * 100
# 5: (1-L) log * 100
# 6: (1-L) * (1-L^{12}) * log * 100 (Prices)

# Transforms the data
#if transf = 0, remove the variable from the panel
print(data)
data <- data[:,transf!=0]
series = series(transf~=0)
transf = transf(transf~=0)