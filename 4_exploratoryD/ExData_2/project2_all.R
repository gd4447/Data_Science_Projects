library(RColorBrewer)
library(ggplot2)
# Data files must be in the working directory.
# For speed: Plot functions check if data is already in the global env.
# If not, it is obtained via emissions_data() function.

emissions_data <- function(path = "./"){
    pm25 <- readRDS("summarySCC_PM25.rds")
    scc <- readRDS("Source_Classification_Code.rds")
    all_data <<- list(pm25, scc)
    return(all_data)
}

plot1 <- function(func = emissions_data){
    if(exists("all_data",where = 1)){
        pm25 <- all_data[[1]]}
    else({
        all_data <- func()
        pm25 <- all_data[[1]]})
    
    pm25$year <- as.factor(pm25$year)
    plot_data <- tapply(pm25$Emissions, pm25$year, sum)
    
    #base plotting
    png("plot1.png", width=480, height=480)
    barplot(plot_data, col = brewer.pal(4,"YlGnBu"), 
            ylab = "Emmisions (Tons)", main = "Total PM2.5 Emissions in the US")
    dev.off()

}
#Plot 2 
plot2 <- function(func = emissions_data){
    if(exists("all_data",where = 1)){
        pm25 <- all_data[[1]]}
    else({
        all_data <- func()
        pm25 <- all_data[[1]]})
    
    pm25$year <- as.factor(pm25$year)
    sub2 <- subset(pm25, fips == "24510")
    plot_data <- tapply(sub2$Emissions, sub2$year, sum)
    
#base plotting
    png("plot2.png", width=480, height=480)
    barplot(plot_data, col = brewer.pal(4,"PuBuGn"), 
            ylab = "Emmisions (Tons)", main = "Total PM2.5 Emissions in Baltimore, MD")
    dev.off()
}

plot3 <- function(func = emissions_data){
    # for speed: If data is already in the global env., use it. If not, obtain it via emissions_data().
    if(exists("all_data",where = 1)){
        pm25 <- all_data[[1]]}
    else({
        all_data <- func()
        pm25 <- all_data[[1]]})
    
    pm25$type <- as.factor(pm25$type)
    sub3 <- subset(pm25, fips == "24510")
    
#ggplot2
    png("plot3.png",width=600,height=480)
    g <- ggplot(sub3, aes(factor(year),Emissions, fill = factor(year)))
    g<- g + 
        geom_bar(stat="identity") +
        facet_grid(.~type) +
        theme_gray(base_size = 14) +
        xlab("") + ylab("Emissions (Tons)") +
        ggtitle("PM2.5 Emissions by Source Type in Baltimore, MD (1999-2008)")
    print(g)
    dev.off()
}

plot4 <- function(func = emissions_data){
    if(exists("all_data",where = 1)){
        pm25 <- all_data[[1]]
        scc <- all_data[[2]]}
    else({
        all_data <- func()
        pm25 <- all_data[[1]]
        scc <- all_data[[2]]})
    
    coal_sources <- grep("Coal", scc$EI.Sector, ignore.case = T)
    sccCoal <- scc[coal_sources,1]
    
    pm25Coal<- subset(pm25, pm25$SCC %in% sccCoal)
    plot_data <- tapply(pm25Coal$Emissions, pm25Coal$year, sum)
    plot_data <- data.frame(year = names(plot_data), Emissions = plot_data) 
    
#ggplot2
    png("plot4.png",width=480,height=480)
    g <- ggplot(plot_data, aes(year,Emissions, fill = factor(year)))
    g<- g + 
        geom_bar(stat="identity") +
        scale_fill_manual(values = brewer.pal(4,"Paired")) +
        theme_gray(base_size = 14) +
        xlab("") + ylab("Emissions (Tons)") +
        ggtitle("PM2.5 Emissions from Coal Sources (1999-2008)")
    print(g)
    dev.off()
}

plot5 <- function(func = emissions_data){
    if(exists("all_data",where = 1)){
        pm25 <- all_data[[1]]
        scc <- all_data[[2]]}
    else({
        all_data <- func()
        pm25 <- all_data[[1]]
        scc <- all_data[[2]]})
    
    mv_sources <- grep("vehicle", scc$EI.Sector, ignore.case = T)
    sccMV <- scc[mv_sources,1]
    
    sub5 <- subset(pm25, fips == "24510")
    pm25mv <- subset(sub5, sub5$SCC %in% sccMV)
    plot_data <- tapply(pm25mv$Emissions, pm25mv$year, sum)
    plot_data <- data.frame(year = names(plot_data), Emissions = plot_data)
    
#ggplot2
    png("plot5.png",width=480,height=480)
    g <- ggplot(plot_data, aes(year,Emissions, fill = factor(year)))
    g<- g + 
        geom_bar(stat="identity") +
        scale_fill_manual(values = brewer.pal(4,"Dark2")) +
        theme_gray(base_size = 14) +
        xlab("") + ylab("Emissions (Tons)") +
        ggtitle("PM2.5 Emissions from \"Motor Vehicle\" Sources 
        in Baltimore, MD (1999-2008)")
    print(g)
    dev.off()
}

plot6 <- function(func = emissions_data){
    if(exists("all_data",where = 1)){
        pm25 <- all_data[[1]]
        scc <- all_data[[2]]}
    else({
        all_data <- func()
        pm25 <- all_data[[1]]
        scc <- all_data[[2]]})
    
    mv_sources <- grep("vehicle", scc$EI.Sector, ignore.case = T)
    sccMV <- scc[mv_sources,1]
    
    sub6<- subset(pm25, (fips %in% c("24510","06037")))
    pm25mv <- subset(sub6, sub6$SCC %in% sccMV)
    
    splitData <- split(pm25mv, list(pm25mv$year, pm25mv$fips))
    calc_data <- lapply(splitData, function(x) sum(x$Emissions))
    
    emissions <- as.vector(unlist(calc_data))
    cols12 <- data.frame(matrix(unlist(strsplit(names(calc_data), "[.]")), ncol=2, byrow=T))
    plot_data <- data.frame(Year = cols12[,1], 
                            Location = cols12[,2],
                            Emissions = emissions)
    plot_data$Location <- ifelse(plot_data$Location == "06037","Los Angeles","Baltimore")

    
#ggplot2
    png("plot6.png",width=540,height=480)
    g <- ggplot(plot_data, aes(factor(Year),Emissions, fill = factor(Location)))
    g<- g + 
        geom_bar(stat="identity") +
        facet_grid(.~Location) +
        scale_fill_manual(values = brewer.pal(4,"RdGy")) +
        theme_gray(base_size = 14) +
        xlab("") + ylab("Emissions (Tons)") +
        ggtitle("  PM2.5 Emissions from \"Motor Vehicle\" Sources
        in Baltimore, MD and Los Angeles, CA (1999-2008)")
    print(g)
    dev.off()
}