#' PISA weight multilevel transform function
#' 
#' This function takes the final student weight and transforms it to be usable
#' in multilevel analyses as per the PISA data analysis manual. The final student
#' weights are transformed in a way that the sum of weights per each country is 
#' constant and equal to the average number of respondents per country ("multilvl_wt"). It also
#' computes transformation of the final student weight where the total sum of
#' weights is equal to the number of respondents in the data frame (i.e., the 
#' sum of weights for each country equals the number of respondents in that country)
#' 
#' @param data Here input the data frame which contains the weights.
#' @param weight String containing the name of the weight variable. Defaults to W_FSTUWT.
#' @param CNTlabel String containing the name of the variable with country labels/names. Defaults to CNT.
#' @return Returns the original data frame with a "multilvl_wt" column where transformed weights are located. It also provides "std_wt" column which is an intermediate step for calculation of "multilvl_wt". These weights are a transformation of the final student weight in a way that the total sum of all weights equals the number of respondents in the whole data frame (i.e. the sum of weights for each country is the number of respondents in that country).
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
    data$std_wt[data[,c(CNTlabel)]==var] = (data[,c(weight)][data[,c(CNTlabel)]==var]/wt_sum)*cnt_lngth #transforms final student weights so that the total sum of weights is equal to the number of respondents
    data$multilvl_wt[data[,c(CNTlabel)]==var] = (data$std_wt[data[,c(CNTlabel)]==var])*(total_cases/length(cntlist)/cnt_lngth) #transforms weights from the previous line so that the sum for each country is constant and equal to an average number of students per country in the data frame
  }
  return(data)
}
