best <- function( state, outcome){
    fileName <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomeVect <- c("heart attack","heart failure", "pneumonia")
    if( !(state %in% state.abb)){
        stop("invalid state")}
    if( !(outcome %in% outcomeVect)){
        stop("invalid outcome")}
    
    if( outcome == outcomeVect[1]){
        data <- fileName[,c(2,7,11)]}
    else if( outcome == outcomeVect[2]){
        data <- fileName[,c(2,7,17)]}
    else if( outcome == outcomeVect[3]){
        data <- fileName[,c(2,7,23)]}
    
    outputData <- subset(data, data$State == state)
    outputData[,3] <- suppressWarnings(as.numeric(outputData[,3]))
    minRows <- outputData[which(outputData[,3] == min(outputData[,3], na.rm = TRUE)),]
    minRows <- sort(minRows$Hospital.Name)
    print(minRows)
    bestHospital <- as.character(minRows[1])
    #print(bestHospital)
}