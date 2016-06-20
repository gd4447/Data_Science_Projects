# corr() takes a directory of data files and a threshold for complete cases and
# calculates the correlation between sulfate and nitrate for files where the
# number of completely observed cases (on all variables) is greater than the
# threshold. The function returns a vector of correlations. If no files meet the
# threshold requirement, then the function returns c().

#Try To improve speed for round2(paid) of the class.
 #idea: apply the threshold sooner. use a counter

corr <- function(directory, threshold = 0){
    fileNames <- list.files(directory, full.names = TRUE)
    
    result_vector <- vector(mode = "numeric", length = 0)
    for (i in seq_along(fileNames)){
        temp_file <- read.csv(fileNames[i])
        goodVals <- complete.cases(temp_file)
        count <- sum(goodVals)
        
        if (count > threshold){
            temp_table <- temp_file[goodVals,]
            corr <- cor(temp_table[ ,"sulfate"], temp_table[ ,"nitrate"])
            result_vector[i] <- corr
        }
    }
    bad <- is.na(result_vector)
    result_vector <- result_vector[!bad]
} 