#' T-value Stars Function
#'
#' This function takes in a number and its corresponding t-value.
#' Returns a number with attached stars based on significance level.
#' p < 0.001 ***, p < 0.01 **, p < 0.05 *, p < 0.1 . or without stars if the p >= 0.1.
#' It is used, for example, when you run a model which gives you estimate together
#' with its t-value without star symbols that represent the significance level.
#' It is an auxiliary function for easy making of APA style tables.
#'
#' @param value Number containing the value (e.g. estimate).
#' @param tval Number containing the t-value associated with value number.
#' @param decplaces Number which represents the rounding of numbers to a specific decimal place. Defaults to 3.
#' @return String with the value input and stars assigned passed on significance level, rounded to the specified number of decimal places.
#' @export
tval.Stars <- function (value,tval,decplaces=3){
convTval <- tval.pval.conv(tval)

if(convTval<0.001){
    return(paste(format(round(value, digits = decplaces), nsmall=decplaces),"***",sep=""))
    } else if (convTval<0.01){
    return(paste(format(round(value, digits = decplaces), nsmall=decplaces),"**",sep=""))
    } else if (convTval<0.05){
    return(paste(format(round(value, digits = decplaces), nsmall=decplaces),"*",sep=""))
    } else if (convTval<0.1){
    return(paste(format(round(value, digits = decplaces), nsmall=decplaces),".",sep=""))
    } else {
    return(format(round(value, digits = decplaces), nsmall=decplaces))
    }
 
}