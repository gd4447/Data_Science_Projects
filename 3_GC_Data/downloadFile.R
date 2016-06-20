downloadFile <- function(fileUrl, folder = "./", fileName = "quiz4q2.csv", ...){
    if(!file.exists(folder)){dir.create(folder)}
    destFile<- paste0(folder,"/", fileName)
    download.file(fileUrl, destfile = destFile) #,mode = "wb")
    wkg_file <<- read.csv("quiz4q2.csv", skip = 5, col.names = 
                             c("countryCode", "rank", "", "Name", "gdp", "a" ,"b","c","d","e"))
    #wkg_file<<- readJPEG(destFile, native = TRUE)
    
}