# complete() reads directory full of files and reports the number of completely
# observed cases in each data file. The function returns a data frame where the
# first column is the name of the file and the second column is the number of
# complete cases.

#The count1 is quicker.Present method is likely to be needed

complete <- function(directory, id = 1:332){
    fileNames <- list.files(directory, full.names = TRUE)
    result_table <- data.frame()
    for (i in id){
        temp_file <- read.csv(fileNames[i])
        goodVals <- complete.cases(temp_file)
        count <- sum(goodVals)
        current_count <- c(i, count)
        result_table <- rbind(result_table, current_count)
    }
    colnames(result_table) <- c("id", "nobs")
    print(result_table)
} 