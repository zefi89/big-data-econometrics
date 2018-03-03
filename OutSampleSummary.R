load("OutSample.Rda")

#########################################
# Compute the mean squared forecast error
#########################################

nn <- c("IPS10", "PUNEW")
nntrue <- c(paste(nn[1], "_true", sep=""), paste(nn[2], "_true", sep=""))

MSFE_RW <- mean((data[,nntrue[1]]-data[,paste(nn[1], "_rw", sep="")])^2)
MSFE_RIDGE <- mean((data[,nntrue[1]]-data[,paste(nn[1], "_ridge_IN0p4", sep="")])^2)

print(MSFE_RIDGE / MSFE_RW)

