# main script function to perform entire analysis
runPlot <- function()
{
        # set global project directory
        mainDir <- "C:/PeerAssign1"
        dir.create(mainDir)
        setwd(mainDir)
        filename <- "household_power_consumption.txt"
        dfPowerData <- getPowerData(filename)
        plot4(dfPowerData)
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
        df2$Voltage = as.numeric(as.character(df2$Voltage))
        df2$Global_reactive_power = as.numeric(as.character(df2$Global_reactive_power))
        df2
}
plot4<-function(df)
{
        png(filename="plot4.png")
        par(mfrow=c(2,2))
        plot4a(df)
        plot4b(df)
        plot4c(df)
        plot4d(df)
        dev.off()
}
plot4a<-function(df)
{
        with(df, plot(DateTime, Global_active_power, type="n", xlab="", ylab="Global Active Power"))
        with(df, lines(DateTime, Global_active_power))
}
plot4b<-function(df)
{
        with(df, plot(DateTime, Voltage, type="n"))
        with(df, lines(DateTime, Voltage))
}
plot4c<-function(df)
{
        with(df, plot(DateTime, Sub_metering_1, type="n", xlab="", ylab="Energy Sub metering"))
        with(df, lines(DateTime, Sub_metering_1, col="black"))
        with(df, lines(DateTime, Sub_metering_2, col="red"))
        with(df, lines(DateTime, Sub_metering_3, col="blue"))
        legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3" ), lty=1, col=c("black", "red", "blue"), bty="n")
}
plot4d<-function(df)
{
        with(df, plot(DateTime, Global_reactive_power, type="n"))
        with(df, lines(DateTime, Global_reactive_power))
}
