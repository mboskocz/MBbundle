#' Weighted Mean of PISA Scores per Group Function
#'
#' This function computes the weighted mean of PISA scores which are computed 
#' based on all Plausible Values for each unique country and returns a data frame with
#' weighted scores for each country and number of observations grouped by a variable. 
#' This function assumes that Plausible Values are labeled as PVXSUBJECT (e.g. PV1MATH, PV2SCIE...).
#'
#' @param data Here input the data frame which contains Plausible Values and weights.
#' @param PVtype String which contains information on which Plausible Values you want to use (e.g. math, scie...). 
#' @param weight String containing the name of the weight variable. Defaults to W_FSTUWT.
#' @param PVnumber Number which represents the number of Plausible Values. Defaults to 10.
#' @param GroupLabel String containing the name of the variable that will serve as a grouping variable (e.g., CNT, gender...). Defaults to CNT.
#' @return Data frame which contains a list of countries each with a weighted score and number of observations.
#' @export
Group.PV.wtd.mean <- function(data,PVtype,weight="W_FSTUWT",PVnumber=10,GroupLabel="CNT") {
`%>%` <- magrittr::`%>%`
grouplist <- sort(unique(data[,c(GroupLabel)]))
result = data.frame(matrix(nrow = length(grouplist), ncol = 3))
lbl <- paste("wtd_mean_",toupper(PVtype),sep="") #label for the weighted mean in the final frame
colnames(result) <- c(GroupLabel, lbl,"N")
counter <- 1
for (var in grouplist){
    helpdf <- data %>% dplyr::filter (get(GroupLabel) == var)
    result[counter,1] <- var
    result[counter,2] <- PV.wtd.mean(helpdf,PVtype,weight,PVnumber) #call calculation of wtd. mean for plausible values
    result[counter,3] <- length(helpdf[,c(GroupLabel)])
    counter <- counter+1
    }
return(result)
}
