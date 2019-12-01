# Exploratory Data Analysis - Week1
# to run this code:
# source("plot4.R")

getData <- function(){
    file <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    download.file(file, destfile = "household_power_consumption.zip")
    unzip("household_power_consumption.zip")
    
}

readData <- function(){
    consumption <- read.table(text = grep("^[1,2]/2/2007",readLines("household_power_consumption.txt"),value=TRUE), sep = ';', col.names = c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), na.strings = '?')
   
}

createGraph <- function(){
    consumption$Date <-as.Date(consumption$Date,format = '%d/%m/%Y')
    consumption$DateTime <- as.POSIXct(paste(consumption$Date, consumption$Time))
    Sys.setlocale(category = "LC_ALL", locale = "english")
    png("plot4.png", width = 480, height = 480, units='px')
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 2), oma = c(1, 1, 0, 0))
    with(consumption, {
        plot(DateTime, Global_active_power,type = 'l',ylab = 'Global Active Power', xlab = " ")  
        plot(DateTime, Voltage,type = 'l',ylab = 'Voltage', xlab = "datetime")
        plot(DateTime, Sub_metering_1, type = 'l',ylab = 'Energy sub metering', xlab = " ")
        lines(DateTime, Sub_metering_2, col="red")
        lines(DateTime, Sub_metering_3, col="blue")
        legend("topright", col=c("black","red","blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lwd=1)
        plot(DateTime, Global_reactive_power,type = 'l', xlab = "datetime")
    })
    
    
    dev.off()
}

getData()
consumption <- readData()
createGraph()
