# main script function to perform entire analysis
runPlot <- function()
{
        # set global project directory
        mainDir <- "C:/PeerAssign1"
        dir.create(mainDir)
        setwd(mainDir)
        filename <- "household_power_consumption.txt"
        dfPowerData <- getPowerData(filename)
        plot3(dfPowerData)
}
getPowerData<-function(fileName)
{
        df<-read.table(fileName, header = TRUE, sep=";")
        library(lubridate)
        df$Date = dmy(df$Date)
        df$Time = hms(df$Time)
        df2 <- df[df$Date >= as.Date("2007-02-01") & df$Date <= as.Date("2007-02-02"), ]
        df2$DateTime <- with(df2, Date + Time)
        df2$Global_active_power = as.numeric(as.character(df2$Global_active_power))
        df2$Sub_metering_1 = as.numeric(as.character(df2$Sub_metering_1))
        df2$Sub_metering_2 = as.numeric(as.character(df2$Sub_metering_2))
        df2$Sub_metering_3 = as.numeric(as.character(df2$Sub_metering_3))
        df2
}
plot3<-function(df)
{
        png(filename="plot3.png")
        with(df, plot(DateTime, Sub_metering_1, type="n", xlab="", ylab="Energy Sub metering"))
        with(df, lines(DateTime, Sub_metering_1, col="black"))
        with(df, lines(DateTime, Sub_metering_2, col="red"))
        with(df, lines(DateTime, Sub_metering_3, col="blue"))
        legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3" ), lty=1, col=c("black", "red", "blue"))
        dev.off()
}