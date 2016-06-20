rankhospital <- function ( state, outcome, num){
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
    sortedData <- outputData[order(outputData[,3],outputData$Hospital.Name),]
    
    notNAs <- complete.cases(sortedData)
    sortedData <- sortedData[notNAs,]
    sortedData$Rank <- 0
    
    for( i in seq_len(nrow(sortedData))){
        sortedData[i,4] <- i}
    
    if(num == "best") num = 1
    if(num == "worst") num = nrow(sortedData)
    if(num > nrow(sortedData)){
        output <- NA
        print(output)}
    else{
        output <- sortedData[num,1]
        print(output)
    }
}
