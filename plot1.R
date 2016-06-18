# main script function to perform entire analysis
runPlot <- function()
{
        # set global project directory
        mainDir <- "C:/PeerAssign1"
        dir.create(mainDir)
        setwd(mainDir)
        filename <- "household_power_consumption.txt"
        dfPowerData <- getPowerData(filename)
        plot1(dfPowerData)
}
getPowerData<-function(fileName)
{
        df<-read.table(fileName, header = TRUE, sep=";")
        library(lubridate)
        df$Date = dmy(df$Date)
        df$Time = hms(df$Time)
        df$Global_active_power = as.numeric(as.character(df$Global_active_power))
        df2 <- df[df$Date >= as.Date("2007-02-01") & df$Date <= as.Date("2007-02-02"), ]
        df2
}
plot1<-function(df)
{
        png(filename="plot1.png")
        with(df, hist(Global_active_power, breaks=c(0, .5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5), freq=TRUE, right=FALSE, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)"))
        dev.off()
}