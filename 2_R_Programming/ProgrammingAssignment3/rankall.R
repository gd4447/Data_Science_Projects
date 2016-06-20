rankall <- function ( outcome, num = "best"){
    fileName <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomeVect <- c("heart attack","heart failure", "pneumonia")
    if( !(outcome %in% outcomeVect)){
        stop("invalid outcome")}
    
    if( outcome == outcomeVect[1]){
        data <- fileName[,c(2,7,11)]}
    else if( outcome == outcomeVect[2]){
        data <- fileName[,c(2,7,17)]}
    else if( outcome == outcomeVect[3]){
        data <- fileName[,c(2,7,23)]}
    
    data[,3] <- suppressWarnings(as.numeric(data[,3]))
    goodVals <- complete.cases(data)
    data <- data[goodVals,]
    
    outputData <- split(data,data$State)
    sortedData <-lapply(outputData, function(x) x[order(x[,3],x$Hospital.Name),])
    
    finalOutput <- data.frame()
    for( i in seq_along(sortedData)){
        nnum <- num
        outputSubset <- sortedData[[i]]
        
        if(nnum == "best") nnum = 1
        if(nnum == "worst") nnum = nrow(outputSubset)
        if(nnum > nrow(outputSubset)){
            output <- data.frame(NA, outputSubset[1,2])
            names(output) <- c("hospital","state")}
        else{
            output <- outputSubset[nnum,1:2]
            names(output) <- c("hospital","state")
        }
        finalOutput <- rbind(finalOutput, output)
    }
    finalOutput
}