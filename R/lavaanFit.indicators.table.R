#' Table of Fit Indicators of lavaan Models Function
#'
#' This function evaluates the result of lavaan fitMeasures function based on Papi (2010) 
#' @param fitMeasures Here input the outcome of the lavaan::fitMeasures function
#' @return Data frame with the values of fit indicators and their evaluation
#' @references Papi, M. (2010). The L2 motivational self system, L2 anxiety, and motivated behavior: A structural equation modeling approach. \emph{System, 38}(3), 467–479. \doi{10.1016/j.system.2010.06.011}
#' 
#' @export

lavaanFit.indicators.table <- function(fitMeasures){
  #source https://doi.org/10.1016/j.ijer.2023.102302
  result = data.frame(matrix(nrow = 13, ncol = 4))
  colnames(result) <- c("Index","Current level","Accepted level","Evaluation")
  result[1] <- c("chiSq pval","chiSQ/df","GFI","AGFI","NFI","NNFI","CFI","RMSEA","SRMR","RFI","TLI","PNFI","IFI")
  result[3] <- c(">.05","<3.0",">.95",">.90",">.90",">.90",">.90","<.05","<.08",">.90",">.90",">.50",">.90")
  
  result[1,2] <- fitMeasures[["pvalue"]]
  result[2,2] <- fitMeasures[["chisq"]]/fitMeasures[["df"]]
  result[3,2] <- fitMeasures[["gfi"]]
  result[4,2] <- fitMeasures[["agfi"]]
  result[5,2] <- fitMeasures[["nfi"]]
  result[6,2] <- fitMeasures[["nnfi"]]
  result[7,2] <- fitMeasures[["cfi"]]
  result[8,2] <- fitMeasures[["rmsea"]]
  result[9,2] <- fitMeasures[["srmr"]]
  result[10,2] <- fitMeasures[["rfi"]]
  result[11,2] <- fitMeasures[["tli"]]
  result[12,2] <- fitMeasures[["pnfi"]]
  result[13,2] <- fitMeasures[["ifi"]]
  
  result[1,4] <- ifelse(result[1,2]>.05,c("Satisfactory"),c("poor"))
  result[2,4] <- ifelse(result[2,2]<3,c("Satisfactory"),c("poor"))
  result[3,4] <- ifelse(result[3,2]>.95,c("Satisfactory"),c("poor"))
  result[4,4] <- ifelse(result[4,2]>.90,c("Satisfactory"),c("poor"))
  result[5,4] <- ifelse(result[5,2]>.90,c("Satisfactory"),c("poor"))
  result[6,4] <- ifelse(result[6,2]>.90,c("Satisfactory"),c("poor"))
  result[7,4] <- ifelse(result[7,2]>.90,c("Satisfactory"),c("poor"))
  result[8,4] <- ifelse(result[8,2]<.05,c("Satisfactory"),c("poor"))
  result[9,4] <- ifelse(result[9,2]<.08,c("Satisfactory"),c("poor"))
  result[10,4] <- ifelse(result[10,2]>.90,c("Satisfactory"),c("poor"))
  result[11,4] <- ifelse(result[11,2]>.90,c("Satisfactory"),c("poor"))
  result[12,4] <- ifelse(result[12,2]>.50,c("Satisfactory"),c("poor"))
  result[13,4] <- ifelse(result[13,2]>.90,c("Satisfactory"),c("poor"))

  return(result)
}
  