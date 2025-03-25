#' Weighted Mean of PISA Scores per Country Function
#'
#' This function computes the weighted mean of PISA scores which are computed 
#' based on all Plausible Values for each unique country and returns a dataframe with the list of 
#' countries, weighted scores for each country and number of observations. 
#' This function assumes that Plausible Values are labeled as PVXSUBJECT (e.g. PV1MATH, PV2SCIE...).
#'
#' @param data Here input the dataframe which contains Plausible Values and weights.
#' @param PVtype String which contains information on which Plausible Values you want to use (e.g. math, scie...). 
#' @param weight String containing the name of the weight variable. Defaults to W_FSTUWT.
#' @param PVnumber Number which represents the number of Plausible Values. Defaults to 10.
#' @param CNTlabel String containg the name of the varaible with country labels/names. Defaults to CNT.
#' @return Dataframe which contains a list of countries each with a weighted score and number of observations.
#' @export
CNT.PV.wtd.mean <- function(data,PVtype,weight="W_FSTUWT",PVnumber=10,CNTlabel="CNT") {
`%>%` <- magrittr::`%>%`
cntlist <- sort(unique(data[,c(CNTlabel)]))
result = data.frame(matrix(nrow = length(cntlist), ncol = 3))
lbl <- paste("wtd_mean_",toupper(PVtype),sep="") #label for the weighted mean in the final frame
colnames(result) <- c("CNT", lbl,"N")
counter <- 1
for (var in cntlist){
    helpdf <- data %>% dplyr::filter (get(CNTlabel) == var)
    result[counter,1] <- var
    result[counter,2] <- PV.wtd.mean(helpdf,PVtype,weight,PVnumber) #call calculation of wtd. mean for plausible values
    result[counter,3] <- length(helpdf[,c(CNTlabel)])
    counter <- counter+1
    }
return(result)
}
