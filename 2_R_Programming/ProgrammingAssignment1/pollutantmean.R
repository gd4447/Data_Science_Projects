# pollutantmean() calculates the mean of a pollutant (sulfate or nitrate) across
# a specified list of monitors. The function takes three arguments: 'directory',
# 'pollutant', and 'id'. Given a vector monitor ID numbers, the function reads
# that monitors' particulate matter data from the 'directory' argument and
# returns the mean of the pollutant across all of the specified files, ignoring
# any NA values.

pollutantmean <- function(directory, pollutant, id = 1:332){
    fileNames <- list.files(directory, full.names = TRUE)
    big_table <- data.frame()
    for (i in id){
        big_table <- rbind(big_table, read.csv(fileNames[i]))   
    }
    meanVal <- mean(big_table[ ,pollutant], na.rm = TRUE)
    print(round(meanVal, digits = 3))
}    
  
