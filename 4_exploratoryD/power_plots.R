library(dplyr)
library(lubridate)
# Data file must be in the working directory.

power_plots <- function(path = "./"){
    data <- read.table("household_power_consumption.txt",
                       header = TRUE, sep = ";", colClasses = "character")
    plotData <<- data %>% 
        subset(Date=="1/2/2007"|Date=="2/2/2007") %>%
        mutate(timestamp = dmy_hms(paste(as.character(Date), as.character(Time), sep = " "))) %>%
        select(timestamp, Global_active_power:Sub_metering_3) %>%
        mutate_each(funs(as.numeric), -timestamp) %>%
        arrange(timestamp) %>% tbl_df %>% return
}
    
plot1 <- function(func = power_plots){
    hist(p1data$Global_active_power, main = "Global Active Power",
         col = "red", xlab = "Global Active Power (kilowatts)")
    title(main = "Global Active Power")
    }

plot2 <- function(func = power_plots){
    p2data <- func()
    with(p2data,
         plot(timestamp, Global_active_power, type="l",
              xlab = NA,
              ylab = "Global Active Power (kilowatts)")
         
)}
 
plot3 <- function(func = power_plots){
    p3data <- func()
    with(p3data,
         plot(timestamp, Sub_metering_1, type="l", 
              xlab = NA,
              ylab = "Energy sub metering"
              ))
    with(p3data, lines(timestamp, Sub_metering_2, col = "red", lwd = 1))
    with(p3data, lines(timestamp, Sub_metering_3, col = "blue", lwd = 1))
    legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
           lty = c(1,1,1), col = c("black","red","blue"), lwd = c(1,1,1))
}

plot4 <- function(func = power_plots){
    p4data <- func()
    par(mfcol = c(2,2))
    
    with( p4data,{
    #Plot1     
        plot(timestamp, Global_active_power, type="l", xlab = NA, ylab = "Global Active Power (kilowatts)")
         
    #Plot 2
        plot(timestamp, Sub_metering_1, type="l", xlab = NA, ylab = "Energy sub metering")
        lines(timestamp, Sub_metering_2, col = "red", lwd = 1)
        lines(timestamp, Sub_metering_3, col = "blue", lwd = 1)
        legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
                lty = c(1,1,1), col = c("black","red","blue"), lwd = c(1,1,1))
    
    #Plot 3
        plot(timestamp, Voltage, type="l", xlab = NA)

    #Plot 4
        plot(timestamp, Global_reactive_power, type="l", xlab = NA)
    })
}