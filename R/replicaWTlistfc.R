#' Replicate Weight List Function
#'
#' This is an auxiliary function that creates a list of 80 replicate weights 
#' based on the label input.
#' 
#' @param repwt String containing the prefix of the replicate weights variables. Defaults to W_FSTR.
#' @return List with 80 replicate weight labels.
#' @export
replicaWTlistfc <- function(repwt="W_FSTR"){
replicaWTlist <- c()
sqv <- seq(1,80,by=1)
for (var in sqv){
    helpme <- paste(repwt,var,sep="")
    replicaWTlist <- c(replicaWTlist,helpme)
    }
return(replicaWTlist)
}
