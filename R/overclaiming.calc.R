#' Overclaiming Calculations Function
#'
#' This function takes a data frame and computes necessary calculation for overclaiming analysis.
#' After calculation it returns an appended data frame with overclaiming variables.
#' It creates dummy variables, variables for computation of overclaiming indices
#' Proportion of Hits (PH), Proportion of False Alarms (PFA), Index of Accuracy (IA),
#' and Index of Exaggeration (IE).
#' This function does not assume absence of NA codes. It, however, assumes that 
#' the answer codes are in a series (e.g., 1, 2, 3, 4, 5). It also assumes that
#' 1 represents "never heard" answer and 5 being the "most familiar" answer.
#'
#' @param data Here input the data frame which contains all overclaiming items.
#' @param existIt Vector containing strings with variable names for existing items.
#' @param nonexistIt Vector containing strings with variable names for nonexistent items.
#' @param scaleStart Number which represents the code for the start of the answering scale (e.g., "Never heard" coded as 1). Defaults to 1.
#' @param scaleStop Number which represents the code for the end of the answering scale (e.g., "Know it well" coded as 5). Defaults to 5.
#' @return Returns appended imputed data frame which now contains variables needed for computation of overclaiming indices.
#' @export
overclaiming.calc <- function (data,existIt,nonexistIt,scaleStart=1,scaleStop=5) {

#PISA 2012 
#exist <- c("ST62Q01","ST62Q02","ST62Q03","ST62Q06", "ST62Q07", "ST62Q08","ST62Q09", "ST62Q10", "ST62Q12", "ST62Q15", "ST62Q16", "ST62Q17", "ST62Q19")
#nonexist <- c("ST62Q04", "ST62Q11", "ST62Q13")

#PISA 2022
#exist <- c("ST289Q01WA", "ST289Q02JA", "ST289Q04JA", "ST289Q05WA", "ST289Q06JA", "ST289Q07JA", "ST289Q08WA", "ST289Q09WA", "ST289Q10WA", "ST289Q14JA")
#nonexist <- c("ST289Q03WA", "ST289Q11WA")

nmbrExist <- seq(1,length(existIt),by=1)
nmbrNonex <- seq(1,length(nonexistIt),by=1)
nmbrOptions <- seq(scaleStop,scaleStart+1,by=-1)

#dummies for later easy calculation of indices, WIP for usage with other than 5-point scales
listExist <- c()
result = data.frame(matrix(nrow = length(existIt), ncol = 0)) #helper data frame for existing items - here we will store concept_e1_heardXYZ variables for later use in calculation
resultNE = data.frame(matrix(nrow = length(nonexistIt), ncol = 0)) #helper data frame for existing items - here we will store concept_n1_heardXYZ variables for later use in calculation
helpme <- c() 
listExist_list <- c() #here we store column names for result data frame
listNonexist_list <- c() #here we store column names for resultNE data frame
sznnmb <- c() #here we store the individual endings of variables (2345,345,45...)
counter <- 1
for (var in nmbrOptions) {
    helpme <- paste(var,helpme,sep="")
    sznnmb <- c(sznnmb,helpme)
    listExist_list <- c(listExist_list,paste("listExist_",helpme,sep=""))
    listNonexist_list <- c(listNonexist_list,paste("listNonexist_",helpme,sep=""))
    result[,c(listExist_list[counter])] <- NA
    resultNE[,c(listNonexist_list[counter])] <- NA
    counter<-counter+1      
    }
    
sznnmb <- sort(sznnmb) #reorder so that the order is 2345,345...
listExist_list <- sort(listExist_list) #same
listNonexist_list <- sort(listNonexist_list) #same

counter2 <- 1 #counts rows in the helper dataframe
for (var in nmbrExist){ #ready concept_e variables
    helpme <- paste("concept_e",var,sep="")
    data[,c(helpme)] <- data[,c(existIt[var])] #setting up clone variable to destroy NAs
    data[,c(helpme)][data[,c(helpme)]>scaleStop] <- NA  #remove NA answers, just in case
    listExist <- c(listExist,helpme)
    counter <- 1
    for (var2 in sznnmb){
        helpme2 <- paste("concept_e",var,"_heard",var2,sep="") #for each existing concept prepare dummies with heardXYZ endings
        data[,c(helpme2)] <- as.integer(counter<data[,c(helpme)])
        result[counter2,c(paste("listExist_",var2,sep=""))] <- helpme2 #store the name in a helper dataframe
        counter <- counter+1        
        } 
    counter2 <- counter2+1
    }

#dummies for later easy calculation of indices
#same as in the forcycle before, just for nonexisting items
listNonexist <- c()
counter2 <- 1
for (var in nmbrNonex){ #ready concept_e variables
    helpme <- paste("concept_n",var,sep="")
    data[,c(helpme)] <- data[,c(nonexistIt[var])] #setting up clone variable to destroy NAs
    data[,c(helpme)][data[,c(helpme)]>scaleStop] <- NA  #remove NA answers, just in case
    listNonexist <- c(listNonexist,helpme)
    counter <- 1
    for (var2 in sznnmb){
        helpme2 <- paste("concept_n",var,"_heard",var2,sep="")
        data[,c(helpme2)] <- as.integer(counter<data[,c(helpme)])
        resultNE[counter2,c(paste("listNonexist_",var2,sep=""))] <- helpme2
        counter <- counter+1        
        } 
    counter2 <- counter2+1
    }

    
counter <- 1
PH_list <- c() #helper list which contains names of PH_XYZ options for later
PFA_list <- c() #same but PFA
for (var2 in sznnmb){ #here we calculate PH and PFA indices for each heardXYZ option
    helpme <- paste("PH_",var2,sep="")
    PH_list <- c(PH_list,helpme)
    data[,c(helpme)] = rowSums(data[,result[,c(listExist_list[counter])]])/length(existIt)*100 #result gives list of variable names for individual heardXYZ options so that we can easily sum all of the existing concepts
    helpme <- paste("PFA_",var2,sep="")
    PFA_list <- c(PFA_list,helpme)
    data[,c(helpme)] = rowSums(data[,resultNE[,c(listNonexist_list[counter])]])/length(nonexistIt)*100   #same but for nonexistent items
    counter <- counter+1
    }



#calculating individual PH indices
data$PH_a1 = rowSums(data[,c(PH_list)])/length(PH_list) #make an average of individual PH_XYZ options
data$PFA_a1 = rowSums(data[,c(PFA_list)])/length(PH_list) #same but PFA

#calculcating IA and IE indices
data$IA_a1 <- (data$PH_a1-data$PFA_a1)/100
data$IE_a1 <- (data$PH_a1+data$PFA_a1)/2/100

return(data) #returns a dataframe with overclaiming calcuations

}