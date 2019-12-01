# Exploratory Data Analysis - Week1
# to run this code:
# source("plot1.R")

getData <- function(){
    file <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    download.file(file, destfile = "household_power_consumption.zip")
    unzip("household_power_consumption.zip")
    
}

readData <- function(){
    consumption <- read.table(text = grep("^[1,2]/2/2007",readLines("household_power_consumption.txt"),value=TRUE), sep = ';', col.names = c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), na.strings = '?')

}

createGraph <- function(){
    png("plot1.png")
    hist(consumption$Global_active_power,col="red",main="Global Active Power",xlab = 'Global Active Power (kilowatt)')
    dev.off()
}

getData()
consumption <- readData()
createGraph()