#' Weighted Mean of PISA Scores Function
#'
#' This function computes the weighted mean of PISA scores which are computed 
#' based on all Plausible Values.
#' This function assumes that Plausible Values are labeled as PVXSUBJECT (e.g. PV1MATH, PV2SCIE...).
#'
#' @param data Here input the dataframe which contains Plausible Values and weights.
#' @param PVtype String which contains information on which Plausible Values you want to use (e.g. math, scie...). 
#' @param weight String containing the name of the final student weight variable. Defaults to W_FSTUWT.
#' @param PVnumber Number which represents the number of Plausible Values. Defaults to 10.
#' @return Returns weighted mean score.
#' @export
PV.wtd.mean <- function(data,PVtype,weight="W_FSTUWT",PVnumber=10){
#this function presumes that PV are labeled as PVXSUBJECT (e.g. PV1MATH, PV2SCIE...), inserted dataframe must contain PV as well as weight

sqv <- seq(1,PVnumber,by=1)
PVtype <- toupper(PVtype)
PVmeanlist <- c() #list with all means
for (var in sqv){ #itterate based on the number of PV
    PVtext <- paste("PV",var,PVtype,sep="")
    meanPV <- weighted.mean(data[,c(PVtext)],data[,c(weight)])
    PVmeanlist <- c(PVmeanlist,meanPV)   
    } 

meanALL <- mean(PVmeanlist) #make a mean of the means
return(meanALL)
}