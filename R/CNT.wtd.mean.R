#' Weighted Means per Country Function
#'
#' This function computes the weighted means of a variable for all unique 
#' countries in the dataframe and returns a dataframe with the list of 
#' countries, weighted means for each country, weighted standard errors, 
#' number of observations, and optionaly also provides confidence intervals.
#' The function assumes that the variable varMean does not contain NA codes.
#'
#' @param data Here input the dataframe which contains the variable for which you need the country means, final student weight, and replicate weights.
#' @param varMean String containing the name of the variable for which you need to compute the country means.
#' @param CNTlabel String containg the name of the varaible with country labels/names. Defaults to CNT.
#' @param weight String containing the name of the final student weight variable. Defaults to W_FSTUWT.
#' @param showCI Boolean. If true, confidence intervals are shown in the return. Defaults to TRUE.
#' @param alpha Decimal number affecting the confidence interval levels. Defaults to 0.95 (95% confidence intervals).
#' @param repwt String containing the prefix of the replicate weights variables. Defaults to W_FSTR.
#' @return Dataframe which contains a list of countries each with a weighted mean, weighted standard error, number of observations, and optionally the confidence intervals.
#' @export
CNT.wtd.mean <- function (data, varMean, CNTlabel="CNT",weight="W_FSTUWT",showCI=TRUE,alpha=0.95,repwt="W_FSTR"){
`%>%` <- magrittr::`%>%`
repwtlist <- replicaWTlistfc(repwt)
data2 <- data[,c(CNTlabel,varMean,weight,repwtlist)] #simplify
data2 <- stats::na.omit(data2) #get rid of NA answers
cntlist <- sort(unique(data2[,c(CNTlabel)]))
result = data.frame(matrix(nrow = length(cntlist), ncol = 4))
colnames(result) <- c("CNT", "wtd_mean", "wtd_SD","N")
conInt <- stats::qnorm((1-alpha)/2)
textconInt <- "" #if conInt not shown this is empty
if (showCI){
textconInt <- paste("", alpha*100, "% confidence intervals.",sep="")
}
counter <- 1
for (var in cntlist){
	dfhelp <- data2 %>% dplyr::filter (get(CNTlabel) == var)
	result[counter,1] <- var
	result[counter,2] <- stats::weighted.mean(dfhelp[,c(varMean)],dfhelp[,c(weight)])
	result[counter,4] <- length(dfhelp[,c(varMean)])
    result[counter,3] <- wtd.std.error(dfhelp,varMean,weight,repwt)$std_error*sqrt(result[counter,4])
	if (showCI){ 
		result$lowCI[counter] <- result[counter,2]-(conInt*result[counter,3])/sqrt(result[counter,4]) #mean+-conInt*SD/sqrt(N)
		result$highCI[counter] <- result[counter,2]+(conInt*result[counter,3])/sqrt(result[counter,4])
		}
	counter <- counter+1
	}
cat(textconInt,"\n")
return(result)
}