#' Response Time Moving Windows Function
#' 
#' This function performs the response time analysis using the moving window
#' method. This method analyzes response time windows of a set length and moves
#' the windows by a specified jump. This creates overlapping intervals which
#' allows to see how an observed variable develops over time. The smaller the
#' window, the more detailed observation of the development of the variable.
#' This function performs the analysis in individual windows and provides you
#' with a data frame with results.
#'
#' @param data Here input the data frame which contains needed variables.
#' @param timeVarLabel String. The name of the time variable.
#' @param funcName String. Here you enter the name of the function you want to apply on the examined variable in each time window. Include required variables in calling of this function. If data frame is required, the function will automatically pass the data variable.
#' @param timeStop Number. Indicates the end time of your analysis.
#' @param timeStart Number. Indicates the starting time of your analysis. Defaults to 0.
#' @param intLength Number. Indicates the length of the time window. E.g. If your time variable is in milliseconds then value 2000 will mean 2s intervals (e.g. 0-2, 0.1-2.1, etc.). Defaults to 2000.
#' @param windowShift Number. Indicates by how much the window will shift. E.g. If your time variable is in milliseconds then value 100 will mean shift by 0.1s (e.g., 0-2, 0.1-2.1, etc.). Defaults to 100.
#' @param endInclusive Boolean. If TRUE then the time intervals will be inclusive of the end of each interval window (e.g. 0-2s inclusive of 2s but not of 0s). If FALSE, then it is inclusive of the beginning (e.g. 0-2s, inclusive of 0s but not of 2s). Defaults to TRUE.
#' @param ... Add all required arguments for a function in funcName.
#' @return Returns data frame where rows are each time interval window with two (or more) columns - timeInt which shows individual time windows, and funcName which provides the result of the function in funcName applied on data in the examined time window. If the applied function returns more than one parameter, the data frame is expanded to present all parameters.
#'
#' @export
RT.moving.windows <- function (data,timeVarLabel,funcName,timeStop,timeStart=0,intLength=2000, windowShift=100,endInclusive=TRUE,...) {
  `%>%` <- magrittr::`%>%`
  helpme <- seq(timeStart,timeStop-intLength,by=windowShift)
  result = data.frame(matrix(nrow = length(helpme), ncol =2))
  colnames(result) <- c("timeInt",funcName) #timeInt shows the time interval size
  counter <- 1
  for (val in helpme){
    if (endInclusive){ #performs the analysis depending which limit is included in the time interval analysis
      helpdf <- data %>% dplyr::filter (get(timeVarLabel)>val & get(timeVarLabel)<=val+intLength) #e.g., 0-2s inclusive of the 2s, not inclusive of 0s
      result[counter,1] <- paste("(",val," - ",val+intLength,">",sep="")
    } else {
      helpdf <- data %>% dplyr::filter (get(timeVarLabel)>=val & get(timeVarLabel)<val+intLength) #e.g., 0-2s inclusive of the 0s, not inclusive of 2s
      result[counter,1] <- paste("<",val," - ",val+intLength,")",sep="")
    }
    test <- "data" %in% names(formals(get(funcName))) #checks if the called function requires data frame, if yes, passes the data variable
    test2 <- "x" %in% names(formals(get(funcName))) #checks if the called function requires x (for functions like mean or sd)
    if (test){ #if function requires data, it passes the data variable
      arguments <- list(data=helpdf,...)
    } else if (test2){  #if function requires x but not data, it creates an object to pass to the function
      dots <- list(...) #list the extra arguments
      namepass <- dots$x #pass the name of the "x" variable
      objectMake <- helpdf[,namepass] #create an object based on the variable name
      excl_dots <- dots[!names(dots) %in% c("x")] #removes x from extra arguments so that it doesn't double
      if (length(excl_dots)>0){ #checks if the list is empty or not to avoid problems
        arguments <- c(list(x=objectMake),excl_dots)
      } else {
        arguments <- list(x=objectMake)
      }
    }
    else { #if it does not require data or x, it just passes the arguments
      arguments <- list(...)
    }
    funcResult <- do.call(funcName,arguments) #call the function based on the ... and string of a function name
    funcResult_length <- length(funcResult)
    result[counter,2] <- funcResult[[1]]
    if (funcResult_length>1) { #if the function returns a list, it expands the data frame to contain all information
      sqv <- seq(2,funcResult_length,by=1)
      for (var in sqv){
        result[counter,var+1] <- funcResult[[var]]
        colnames(result)[[var+1]] <- names(funcResult[var])
      }
    }
    counter <- counter+1
  }
  return (result)
}

