# adjust for outliers,
# observations with absolute value larger then 10 times the
# interquartile distance are replaced with their median
outliers <- function(x) {
    q = sapply(x, quantile, probs=c(0.25, 0.5, 0.75))

    # define outliers as those obs. that exceed 10 times the interquartile
    # distance
    Jout <- abs(x - q['50%',]) > 10 * abs(q['75%',] - q['25%',])

    z <- x
    z[Jout] = q['50%',] # put the median in place of outliers

    # Replace outliers with centered moving average of order 3.
    zma <- MAcentered(z,3);
    z[Jout] <- z[Jout,]

    return(z)
}

# this function compute MA of order k
MAcentered <- function(x, k_ma) {
    x_ma <- x[NULL,]
    xpad <- rbind(x[rep(1,k_ma),], x, x[rep(NROW(x), k_ma),])
    for (j in (k_ma+1):(NROW(xpad)-k_ma)){
        x_ma[j-k_ma,] <- colMeans(xpad[(j-k_ma):(j+k_ma),])
    }
    return(x_ma)
}

