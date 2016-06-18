# main script function to perform entire analysis
runPlot <- function()
{
        # set global project directory
        mainDir <- "C:/PeerAssign1"
        dir.create(mainDir)
        setwd(mainDir)
        filename <- "household_power_consumption.txt"
        dfPowerData <- getPowerData(filename)
        plot2(dfPowerData)
}
getPowerData<-function(fileName)
{
        df<-read.table(fileName, header = TRUE, sep=";")
        library(lubridate)
        df$Date = dmy(df$Date)
        df$Time = hms(df$Time)
        df2 <- df[df$Date >= as.Date("2007-02-01") & df$Date <= as.Date("2007-02-02"), ]
        df2$Global_active_power = as.numeric(as.character(df2$Global_active_power))
        df2$DateTime <- with(df2, Date + Time)
        df2
}
plot2<-function(df)
{
        png(filename="plot2.png")
        with(df, plot(DateTime, Global_active_power, type="n", xlab="", ylab="Global Active Power (kilowatts)"))
        with(df, lines(DateTime, Global_active_power))
        dev.off()
}