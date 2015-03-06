plot3 <- function(directory=getwd(),filename="household_power_consumption.txt") {
        # get file and directory
        my_file <- paste(directory,filename,sep="/")
        # read the first 5 rows to determine column classes
        my_data <- read.table(my_file, header = TRUE,sep=";",nrows = 5,na.strings="?")
        classes <- sapply(my_data, class)
        # read all data
        my_data <- read.table(my_file, header = TRUE, sep=";", colClasses = classes,na.strings="?")
        # concat date and time together
        my_data$DateTime <- paste(my_data$Date,my_data$Time,sep=" ")
        # convert Date in Date for subsetting
        my_data$Date <- as.Date(my_data$Date,"%e/%m/%Y")
        # convert DateTime for plotting
        my_data$DateTime <- strptime(my_data$DateTime,"%e/%m/%Y %H:%M:%S")
        # subset data with Date =1.2.2007 or Date=2.2.2007
        firstDate <- as.Date("1/2/2007","%e/%m/%Y")
        secondDate <- as.Date("2/2/2007","%e/%m/%Y")
        my_data <- my_data[my_data$Date==firstDate | my_data$Date==secondDate,]
        #print(str(my_data))
        
        # Plot Histogram and write into file
        png(file="plot3.png")
        with(my_data,plot(DateTime,Sub_metering_1,type="n",xlab="",ylab=""))
        with(my_data,lines(DateTime,Sub_metering_1,type="l",col="black",xlab="",ylab=""))
        with(my_data,lines(DateTime,Sub_metering_2,type="l",col="red",xlab="",ylab=""))
        with(my_data,lines(DateTime,Sub_metering_3,type="l",col="blue",xlab="",ylab=""))
        legend("topright", col=c("black","red","blue"),legend=c("Sub_metring_1","Sub_metring_2","Sub_metring_3"),lty=1)
        title(main="",xlab="",ylab="Energy sub metering")
        dev.off()
}