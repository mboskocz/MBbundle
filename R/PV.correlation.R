#' Correlation with PISA Score Function
#'
#' This function computes the (unweighted) correlation of a variable with PISA scores 
#' which are computed based on all Plausible Values.
#' This function assumes that Plausible Values are labeled as PVXSUBJECT (e.g. PV1MATH, PV2SCIE...).
#' It also assumes that variable does not contain NA codes.
#'
#' @param data Here input the data frame which contains the variable for which you need the correlation and Plausible Values.
#' @param cor1 String which contains the variable name for which you want the correlation value.
#' @param PVtype String which contains information on which Plausible Values you want to use – the variable ending (e.g. math, scie, read...). 
#' @param PVnumber Number which represents the number of Plausible Values. Defaults to 10.
#' @param minObserv Number. Represents the minimal number of observations required for the correlation to run. If not reached, returns NA. Defaults to 40.
#' @return List which contains the correlation value, and confidence intervals.
#' @export
PV.correlation <- function(data,cor1,PVtype,PVnumber=10,minObserv=40){
#https://meta-analysis.com/download/Meta-analysis%20Effect%20sizes%20based%20on%20correlations.pdf
  if (sum(!is.na(data[,cor1]))>=minObserv){
    PVtype <- toupper(PVtype)
    
    corList <- c()
    corvarList <- c()
    sqv <- seq(1,PVnumber,by=1)
    for (var in sqv){ #calculate correlation and variance for each Plausible Value
      PVtext <- paste("PV",var,PVtype,sep="")
      testPV <- stats::cor.test(data[,c(cor1)], data[,c(PVtext)])
      corList <- c(corList,testPV$estimate)
      corvar <- ((1-(testPV$estimate)^2)^2)/(testPV$parameter+2-1) #(1-r^2)^2/(n-1)
      corvarList <- c(corvarList,corvar)
    }
    corAVG <- mean(corList) #final correlation is mean of correlations for each PV
    imputationvarCor = 0
    for (var in sqv){
      imputationvarCor = imputationvarCor + (corList[var]-corAVG)^2 #imputation variance is the sum of square differences between PV correlation and final correlation divided by 9
    }
    imputationvarCor = (1/9)*imputationvarCor #variance - (1/M-1)*sum(theta i - theta)^2 https://www.oecd-ilibrary.org/docserver/9789264056275-7-en.pdf?expires=1723555289&id=id&accname=guest&checksum=AAEB995AEC723127E18C0D2AFE568A07
    samplingVar = mean(corvarList) #sampling variance is the mean of variances for each PV
    finalVar = samplingVar + (1+1/10)*imputationvarCor
    
    fm <- ((1+(1/10))*imputationvarCor)/finalVar
    
    degFr <- 80 #degrees of freedom to get the v value needed for final confidence interval
    
    valueV <- 1/(((fm^2)/9)+((1-fm)^2/degFr))
    
    
    finalCI <- abs(stats::qt(.025,valueV))*sqrt(finalVar)
    
    lowCI <- as.numeric(corAVG)-finalCI
    highCI <- as.numeric(corAVG)+finalCI
    
    return(list(corVal=as.numeric(corAVG),lowCI=as.numeric(lowCI),highCI=as.numeric(highCI)))
  } else {
    return(list(corVal=NA,lowCI=NA,highCI=NA))
  }
}
