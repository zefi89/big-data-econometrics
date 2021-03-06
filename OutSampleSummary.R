library(xtable)

table_dir <- "tables"
plot_dir <- "plots"

dir.create(file.path(table_dir), showWarnings = FALSE)
dir.create(file.path(plot_dir), showWarnings = FALSE)

load("OutSample.Rda")

# Scan variable for ridge
IN = c(0.1, 0.2, 0.3 ,0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
INlabel = c("IN0p1", "IN0p2", "IN0p3" ,"IN0p4", "IN0p5", "IN0p6", "IN0p7", "IN0p8", "IN0p9", "cv")
# Scan variable for PC and lasso
K = c(1, 2, 3, 5, 10, 25, 50, 60, 75, 100)
Klabel = c("K1", "K2", "K3", "K5", "K10", "K25", "K50", "K60", "K75", "K100", "cv")
r = c(1, 2, 3, 5, 10, 25, 50, 60, 75, 100)

split_date <- '1985-01-01'
split_i = which(dates$Date == split_date)

nn <- c("IPS10", "PUNEW")

k = 1

for (name in nn) {
    ntrue <- paste(name, "_true", sep="")

    # Compute the MSFE from random walk
    cname = paste(name, "_rw", sep="")
    MSFE_RW <- mean((data[,ntrue]-data[,cname])^2)
    MSFE_RW_I <- mean((data[1:(split_i-1),ntrue]-data[1:(split_i-1),cname])^2)
    MSFE_RW_II <- mean((data[split_i:NROW(data),ntrue]-data[split_i:NROW(data),cname])^2)
    VAR_TRUE <- var(data[,ntrue])

    pdf(file.path(plot_dir, paste(cname, ".pdf", sep="")), width=7, height=5)
    plot(as.Date(dates$Date), data[,ntrue], type='l', xlab="", ylab=paste("transformed", name))
    lines(as.Date(dates$Date), data[,cname], col="red")
    #title(main=paste("Forecasting", paste(name, paste("with Random walk"))))
    dev.off()

    ###############################
    # Make the summary table for PC
    ###############################

    REL_MSFE = c()
    REL_MSFE_I = c()
    REL_MSFE_II = c()
    REL_VAR = c()
    CORR = c()
    for (i in 1:length(r)) {
        cname = paste(paste(name, "_pc_r", sep=""), r[i], sep="")
        MSFE <- mean((data[,ntrue]-data[,cname])^2)
        REL_MSFE[i] = MSFE / MSFE_RW

        MSFE_I <- mean((data[1:(split_i-1),ntrue]-data[1:(split_i-1),cname])^2)
        REL_MSFE_I[i] = MSFE_I / MSFE_RW_I

        MSFE_II <- mean((data[split_i:NROW(data),ntrue]-data[split_i:NROW(data),cname])^2)
        REL_MSFE_II[i] = MSFE_II / MSFE_RW_II

        REL_VAR[i] = var(data[,cname]) / VAR_TRUE

        CORR[i] = cor(data[,cname], data[,paste(name, "_pc_r10", sep="")])

        if (cname == paste(name, "_pc_r10", sep="")) {
            pc_summary = c(REL_MSFE[i], REL_MSFE_I[i], REL_MSFE_II[i], REL_VAR[i], CORR[i])
        }

        pdf(file.path(plot_dir, paste(cname, ".pdf", sep="")), width=7, height=5)
        plot(as.Date(dates$Date), data[,ntrue], type='l', xlab="", ylab=paste("transformed", name))
        lines(as.Date(dates$Date), data[,cname], col="red")
        #title(main=paste("Forecasting", paste(name, paste("with Principal Components (r=", paste(r[i], ")", sep=""), sep=""))))
        dev.off()

    }

    table <- data.frame(REL_MSFE, REL_MSFE_I, REL_MSFE_II, REL_VAR)
    colnames(table) <- c("MFSE 1971 - 2002", "MFSE 1971 - 1984", "MFSE 1985 - 2002", "Variance")
    rownames(table) <- r
    table = t(table)

    sink(paste(table_dir, paste(paste("/PC_", name, sep=""), ".tex", sep=""), sep=""))
    print(xtable(table))
    sink()

    ##################################
    # Make the summary table for Lasso
    ##################################

    REL_MSFE = c()
    REL_MSFE_I = c()
    REL_MSFE_II = c()
    REL_VAR = c()
    CORR = c()
    for (i in 1:length(Klabel)) {
        cname = paste(paste(name, "_lasso_", sep=""), Klabel[i], sep="")
        MSFE <- mean((data[,ntrue]-data[,cname])^2)
        REL_MSFE[i] = MSFE / MSFE_RW

        MSFE_I <- mean((data[1:(split_i-1),ntrue]-data[1:(split_i-1),cname])^2)
        REL_MSFE_I[i] = MSFE_I / MSFE_RW_I

        MSFE_II <- mean((data[split_i:NROW(data),ntrue]-data[split_i:NROW(data),cname])^2)
        REL_MSFE_II[i] = MSFE_II / MSFE_RW_II

        REL_VAR[i] = var(data[,cname]) / VAR_TRUE

        CORR[i] = cor(data[,cname], data[,paste(name, "_pc_r10", sep="")])

        if (i == length(Klabel)) {
            lasso_summary = c(REL_MSFE[i], REL_MSFE_I[i], REL_MSFE_II[i], REL_VAR[i], CORR[i])
        }

        pdf(file.path(plot_dir, paste(cname, ".pdf", sep="")), width=7, height=5)
        plot(as.Date(dates$Date), data[,ntrue], type='l', xlab="", ylab=paste("transformed", name))
        lines(as.Date(dates$Date), data[,cname], col="red")
        #title(main=paste("Forecasting", paste(name, paste("with Lasso (K=", paste(K[i], ")", sep=""), sep=""))))
        dev.off()
    }

    table <- data.frame(c(lambda_lasso[,name], lambda_lasso_best[k]), REL_MSFE, REL_MSFE_I, REL_MSFE_II, REL_VAR, CORR)
    colnames(table) <- c("lambda", "MFSE 1971 - 2002", "MFSE 1971 - 1984", "MFSE 1985 - 2002", "Variance", "Correlation with PC forecasts (r=5)")
    #table <- data.frame(REL_MSFE, REL_MSFE_I, REL_MSFE_II, REL_VAR, CORR)
    #colnames(table) <- c("MFSE 1971 - 2002", "MFSE 1971 - 1984", "MFSE 1985 - 2002", "Variance", "Correlation with PC forecasts (r=10)")
    rownames(table) <- c(K, "cv")
    table = t(table)

    sink(paste(table_dir, paste(paste("/LASSO_", name, sep=""), ".tex", sep=""), sep=""))
    print(xtable(table))
    sink()

    ##################################
    # Make the summary table for Ridge
    ##################################

    REL_MSFE = c()
    REL_MSFE_I = c()
    REL_MSFE_II = c()
    REL_VAR = c()
    CORR = c()
    for (i in 1:length(INlabel)) {
        cname = paste(paste(name, "_ridge_", sep=""), INlabel[i], sep="")
        MSFE <- mean((data[,ntrue]-data[,cname])^2)
        REL_MSFE[i] = MSFE / MSFE_RW

        MSFE_I <- mean((data[1:(split_i-1),ntrue]-data[1:(split_i-1),cname])^2)
        REL_MSFE_I[i] = MSFE_I / MSFE_RW_I

        MSFE_II <- mean((data[split_i:NROW(data),ntrue]-data[split_i:NROW(data),cname])^2)
        REL_MSFE_II[i] = MSFE_II / MSFE_RW_II

        REL_VAR[i] = var(data[,cname]) / VAR_TRUE

        CORR[i] = cor(data[,cname], data[,paste(name, "_pc_r10", sep="")])

        if (i == length(INlabel)) {
            ridge_summary = c(REL_MSFE[i], REL_MSFE_I[i], REL_MSFE_II[i], REL_VAR[i], CORR[i])
        }

        pdf(file.path(plot_dir, paste(cname, ".pdf", sep="")), width=7, height=5)
        plot(as.Date(dates$Date), data[,ntrue], type='l', xlab="", ylab=paste("transformed", name))
        lines(as.Date(dates$Date), data[,cname], col="red")
        #title(main=paste("Forecasting", paste(name, paste("with Ridge (Res.var.=", paste(IN[i], ")", sep=""), sep=""))))
        dev.off()
    }

    table <- data.frame(c(lambda_ridge[,name], lambda_ridge_best[k]), REL_MSFE, REL_MSFE_I, REL_MSFE_II, REL_VAR, CORR)
    colnames(table) <- c("lambda", "MFSE 1971 - 2002", "MFSE 1971 - 1984", "MFSE 1985 - 2002", "Variance", "Correlation with PC forecasts (r=10)")
    rownames(table) <- c(IN, "cv")
    table = t(table)

    sink(paste(table_dir, paste(paste("/RIDGE_", name, sep=""), ".tex", sep=""), sep=""))
    print(xtable(table))
    sink()

    ########################################
    # Make the plots for the regression tree
    ########################################

    cname = paste(name, "_tree", sep="")

    MSFE <- mean((data[,ntrue]-data[,cname])^2)
    REL_MSFE = MSFE / MSFE_RW
    MSFE_I <- mean((data[1:(split_i-1),ntrue]-data[1:(split_i-1),cname])^2)
    REL_MSFE_I = MSFE_I / MSFE_RW_I
    MSFE_II <- mean((data[split_i:NROW(data),ntrue]-data[split_i:NROW(data),cname])^2)
    REL_MSFE_II = MSFE_II / MSFE_RW_II
    REL_VAR = var(data[,cname]) / VAR_TRUE
    CORR = cor(data[,cname], data[,paste(name, "_pc_r10", sep="")])

    tree_summary = c(REL_MSFE, REL_MSFE_I, REL_MSFE_II, REL_VAR, CORR)

    pdf(file.path(plot_dir, paste(cname, ".pdf", sep="")), width=7, height=5)
    plot(as.Date(dates$Date), data[,ntrue], type='l', xlab="", ylab=paste("transformed", name))
    lines(as.Date(dates$Date), data[,cname], col="red")
    #title(main=paste("Forecasting", paste(name, "with Regression tree", sep="")))
    dev.off()

    ######################################
    # Make the plots for the random forest
    ######################################

    cname = paste(name, "_forest", sep="")

    MSFE <- mean((data[,ntrue]-data[,cname])^2)
    REL_MSFE = MSFE / MSFE_RW
    MSFE_I <- mean((data[1:(split_i-1),ntrue]-data[1:(split_i-1),cname])^2)
    REL_MSFE_I = MSFE_I / MSFE_RW_I
    MSFE_II <- mean((data[split_i:NROW(data),ntrue]-data[split_i:NROW(data),cname])^2)
    REL_MSFE_II = MSFE_II / MSFE_RW_II
    REL_VAR = var(data[,cname]) / VAR_TRUE
    CORR = cor(data[,cname], data[,paste(name, "_pc_r10", sep="")])

    forest_summary = c(REL_MSFE, REL_MSFE_I, REL_MSFE_II, REL_VAR, CORR)

    pdf(file.path(plot_dir, paste(cname, ".pdf", sep="")), width=7, height=5)
    plot(as.Date(dates$Date), data[,ntrue], type='l', xlab="", ylab=paste("transformed", name))
    lines(as.Date(dates$Date), data[,cname], col="red")
    #title(main=paste("Forecasting", paste(name, "with Regression tree", sep="")))
    dev.off()

    table <- data.frame(pc_summary, ridge_summary, lasso_summary, tree_summary, forest_summary)
    rownames(table) <- c("MFSE 1971 - 2002", "MFSE 1971 - 1984", "MFSE 1985 - 2002", "Variance", "Correlation with PC forecasts (r=10)")
    colnames(table) <- c("PC=10", "Ridge", "Lasso", "Decision tree", "Random forest")

    sink(paste(table_dir, paste(paste("/TREE_FOREST_", name, sep=""), ".tex", sep=""), sep=""))
    print(xtable(table))
    sink()

    k = k + 1
}
