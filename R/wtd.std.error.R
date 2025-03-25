#' Weighted Standard Error Function
#'
#' This function computes the weighted standard error for PISA variables. It 
#' uses the final student weight and replicate weights for computation according
#' to PISA data analysis manual.
#' The principle of the function is that final mean is computed using the final 
#' student weight and then 80 means (weighted by replicate weights) are computed.
#' Then the sampling variance and standard error are computed.
#' The function assumes that the variable varS does not contain NA codes.
#'
#' @param data Here input the dataframe which contains the variable for which you need standard error, final student weight, and replicate weights.
#' @param varS String containing the name of the variable for which you need to compute the standard error.
#' @param finalwt String containing the name of the final student weight variable. Defaults to W_FSTUWT.
#' @param repwt String containing the prefix of the replicate weights variables. Defaults to W_FSTR.
#' @return List containing the standard error (std_error) and sampling variance (sampVar).
#' @export
wtd.std.error <- function (data,varS,finalwt="W_FSTUWT",repwt="W_FSTR"){
replicaWTlist <- replicaWTlistfc(repwt)
meanfinal <- stats::weighted.mean(data[,c(varS)],data[,c(finalwt)])
replicameanlist <- c()
sqv <- seq(1,80,by=1)
for (var in sqv){ #compute mean for each replicate weight
    meanrep <- stats::weighted.mean(data[,c(varS)],data[,c(replicaWTlist[var])])
    replicameanlist <- c(replicameanlist,meanrep)
    }

squaresum <- 0
for (var in sqv){ #compute sum of sqaured differences replica mean - final mean
    squaresum <- squaresum + (replicameanlist[var]-meanfinal)^2
    }
samplingVariance <- (1/20)*squaresum #sampling varaince is the sum of square differences divided by 20 as per PISA data analysis manual
std_error <- sqrt(samplingVariance) #standard error is the square root of sampling variance
return(list(std_error=std_error,sampVar=samplingVariance))
}