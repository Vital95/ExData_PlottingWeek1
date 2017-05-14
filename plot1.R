loadAndSubset <- function(){
       filePath <- paste(getwd(),"household_power_consumption.txt",sep="/")
       file <- read.table(filePath,sep = ";", header = T)
       data <- file[file$Date == "1/2/2007" | file$Date == "2/2/2007" ,]
       data
}

plot1 <- function(){
       setwd("E:/R/Working_with_R/C 4 w 1 task")
       data <- loadAndSubset()
       par(mar = rep(2, 4))
       hist(as.numeric(data$Global_active_power), main = "Global_active_power", ylab = "frequency", xlab = "Global_active_power", col = "red")
       dev.copy(png,'plot1.png')
       dev.off()
}