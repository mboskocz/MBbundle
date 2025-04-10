#' PISA weight multilevel transform function
#' 
#' This function takes the Final student weight and transforms it to be usable
#' in multilevel analyses. The final student weights are transformed in a way
#' that the sum of weights per each country is constant and equal to the average
#' number of respondents per country.
#' 
#' @param data Here input the data frame which contains the weights.
#' @param weight String containing the name of the weight variable. Defaults to W_FSTUWT.
#' @param CNTlabel String containing the name of the variable with country labels/names. Defaults to CNT.
#' @return Returns data frame with a "multilvl_wt" column where transformed weights are located
#' 
#' @export
PISA.wt.multilvl.transform <- function (data,weight="W_FSTUWT",CNTlabel="CNT")  {
  cntlist <- unique(data[,c(CNTlabel)])
  total_cases <- nrow(data)
  data$std_wt <- NA
  data$multilvl_wt <- NA
  for (var in cntlist){
    wt_sum <- sum(data[,c(weight)][data[,c(CNTlabel)]==var])
    cnt_lngth <- length(data[,c(CNTlabel)][data[,c(CNTlabel)]==var])
    data$std_wt[data[,c(CNTlabel)]==var] = (data[,c(weight)][data[,c(CNTlabel)]==var]/wt_sum)*cnt_lngth
    data$multilvl_wt[data[,c(CNTlabel)]==var] = (data$std_wt[data[,c(CNTlabel)]==var])*(total_cases/length(cntlist)/cnt_lngth)
  }
  return(data)
}
