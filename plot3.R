loadAndSubset <- function(){
       filePath <- paste(getwd(),"household_power_consumption.txt",sep="/")
       file <- read.table(filePath,sep = ";", header = T)
       data <- file[file$Date == "1/2/2007" | file$Date == "2/2/2007" ,]
       data
}

plot3 <- function(){
       library(dplyr)
       setwd("E:/R/Working_with_R/C 4 w 1 task")
       data <- loadAndSubset()
       par(mar = rep(2, 4))
       date <- mutate(data, AllDate = paste(Date, Time))
       vector_of_times <- strptime(date$AllDate, format = "%d/%m/%Y %H:%M:%S")
       vector1 <- vector_of_times[1:(length(vector_of_times)/2)]
       vector2 <- vector_of_times[((length(vector_of_times)/2)+1):length(vector_of_times)]
       data1 <- data[data$Date == "1/2/2007",]
       data2 <- data[data$Date == "2/2/2007",]
       rangeX <- c(as.POSIXct(vector1[1], format = "%d/%m/%Y %H:%M:%S"), as.POSIXct(vector2[length(vector2)], format = "%d/%m/%Y %H:%M:%S"))
       rangeY <- c(min(as.numeric(data1$Sub_metering_1)), max(as.numeric(data1$Sub_metering_1)))
       
       plot(vector1, as.numeric(data1$Sub_metering_1), type = "l", xlim = rangeX, ylim = rangeY)
       points(vector1, as.numeric(data1$Sub_metering_1), col = "green", type = "l")
       points(vector1, as.numeric(data1$Sub_metering_2), col = "red", type = "l")
       points(vector1, as.numeric(data1$Sub_metering_3), col = "blue", type = "l")
       
       par(new=TRUE)
      
       plot(vector2, as.numeric(data2$Sub_metering_1), type = "l", xlim = rangeX,ylim = rangeY)
       points(vector2, as.numeric(data2$Sub_metering_1), col = "green", type = "l")
       points(vector2, as.numeric(data2$Sub_metering_2), col = "red", type = "l")
       points(vector2, as.numeric(data2$Sub_metering_3), col = "blue", type = "l")
       
       legend("topright", pch = 1,col = c("green", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
       
       dev.copy(png,'plot3.png')
       dev.off()
}