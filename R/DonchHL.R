# a modified version of DonchanChannel from TTR package to allow for 
# different lengths for high/low boundaries

DonchHL <- function (HL, nh = 10, nl = 10, include.lag = TRUE) 
{
    HL <- try.xts(HL, error = as.matrix)
    if (!(NCOL(HL) %in% c(1, 2))) {
        stop("Price series must be either High-Low, or Close/univariate.")
    }
    if (NCOL(HL) == 2) {
        hi <- HL[, 1]
        lo <- HL[, 2]
    }
    else {
        hi <- HL
        lo <- HL
    }
    high <- runMax(hi, nh)
    low <- runMin(lo, nl)
    mid <- (high + low)/2
    result <- cbind(high, mid, low)
    colnames(result) <- c("high", "mid", "low")
    if (include.lag) {
        result <- lag.xts(result)
    }
    reclass(result, HL)
}
