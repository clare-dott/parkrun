library(tidyverse)

PlotPDF <- function(CSV, PDF, ylimmin, ylimmax){

  #Set working directory - to be changed if run on another machine
setwd("C:\\Users\\clare\\Documents\\Data Science\\parkrun")

  #Read csv in working directory
parkrunner <- as_tibble(read.csv(CSV))

#Remove dodgy bits from column names
colnames(parkrunner) <- c("Event", "Run.Date", "Run.Number", "Pos", "Time",
                      "Age.Grade", "PB")

#Transform data into something usable
parkrunner <- mutate(parkrunner, Rundate = as.Date(Run.Date, format="%d/%m/%Y"),
                 CTime = str_pad(Time, width=6, side="left", pad=":"),
                 HTime = str_pad(CTime, width=7, side="left", pad="0"),
                 Age.Grade = as.numeric(gsub("%", "", Age.Grade)),
                 Year = format(as.Date(Run.Date, format="%d/%m/%Y"),"%Y"))

#Create number of events column
parkrunner <- arrange(parkrunner, Rundate)
parkrunner$NumEvents <- as.numeric(with(parkrunner, ave(Event, 
                                                FUN=function(x) cumsum(!duplicated(x)))))

parkrunner <- separate(parkrunner, HTime, c("Hours", "Minutes", "Seconds"))

#Transform run time into a numeric minutes format
parkrunner$Hours <- as.numeric(parkrunner$Hours)
parkrunner$Minutes <- as.numeric(parkrunner$Minutes)
parkrunner$Seconds <- as.numeric(parkrunner$Seconds)

parkrunner$TimeM <- ((parkrunner$Hours * 3600) + (parkrunner$Minutes*60) +
                   parkrunner$Seconds) / 60

#Create pace column
#parkrunner$Pace <- parkrunner$TimeM/5 

#Select only columns to be used
parkrunner <- select(parkrunner, Rundate, Year, Event, Run.Number, Pos, Time, TimeM,
                     Age.Grade, PB, NumEvents)



#Create PDF  
  pdf(PDF,
    width = 5.99,
    height = 3.72)

#Scatter plot of date vs time with smoother
plotA <- ggplot(data=parkrunner, aes(x=Rundate, y=TimeM)) +
  geom_point(aes(size=Pos, colour=PB)) +
  geom_smooth() +
  xlab("Run Date") +
  ylab("Run Time (minutes)") +
  ggtitle("Run Times by Date")


#Histogram of run times
plotB <- ggplot(parkrunner, aes(x=TimeM)) +
  geom_histogram(binwidth = 1, fill="cyan4", colour="grey")+
  xlab("Run Time (minutes)") +
  ggtitle("Histogram of Run Times") +
  theme(axis.title = element_text(colour="cyan4"),
        plot.title = element_text(colour="cyan4"),
        axis.text = element_text(colour="cyan4"))

#Histogram of age grade
plotC <- ggplot(parkrunner, aes(x=Age.Grade)) +
  geom_histogram(binwidth = 1, fill="deeppink4", colour="grey") +
  xlab("Age Grade") +
  ggtitle("Histogram of Age Grade") +
  theme(axis.title = element_text(colour="deeppink4"),
        plot.title = element_text(colour="deeppink4"),
        axis.text = element_text(colour="deeppink4"))

#Histogram of attendance
plotD <- ggplot(parkrunner, aes(x=Rundate)) +
  geom_histogram(binwidth = 7, fill="skyblue3") +
  xlab("Run Date") +
  ggtitle("Histogram of parkrun Attendance") +
  theme(axis.title = element_text(colour="skyblue3"),
        plot.title = element_text(colour="skyblue3"),
        axis.text = element_text(colour="skyblue3"))

#Histogram of run number
plotE <- ggplot(parkrunner, aes(x=Run.Number)) +
  geom_histogram(binwidth = 1, fill="turquoise4") +
  xlab("Run Number") +
  ggtitle("Histogram of Run Numbers") +
  theme(axis.title = element_text(colour="turquoise4"),
        plot.title = element_text(colour="turquoise4"),
        axis.text = element_text(colour="turquoise4"))

#Histogram of position
plotF <- ggplot(parkrunner, aes(x=Pos)) +
  geom_histogram(binwidth = 50, fill="seagreen4", colour="grey") +
  xlab("Position") +
  ggtitle("Histogram of Positions") +
  theme(axis.title = element_text(colour="seagreen4"),
        plot.title = element_text(colour="seagreen4"),
        axis.text = element_text(colour="seagreen4"))

#Density plot of run times
plotG <- ggplot(parkrunner, aes(x=TimeM)) +
  geom_density(colour="mediumpurple4", size=2) +
  xlab("Run Time (minutes)") +
  ggtitle("Density Plot of Run Time") +
  theme(axis.title = element_text(colour="mediumpurple4"),
        plot.title = element_text(colour="mediumpurple4"),
        axis.text = element_text(colour="mediumpurple4"))

#Boxplot with jitter of run times by year
plotH <- ggplot(parkrunner, aes(x=Year, y=TimeM, colour=Year)) +
  geom_jitter() +
  geom_boxplot(size=1, alpha=0.5) +
  coord_cartesian(ylim = c(ylimmin,ylimmax)) +
  ylab("Run Time (minutes)") +
  ggtitle("Boxplot of Run Times by Year")

#Boxplot of run number by year
plotI <- ggplot(parkrunner, aes(x=Year, y=Run.Number, colour=Year)) +
  geom_jitter() +
  geom_boxplot(size=1, alpha=0.5) +
  ylab("Run Number") +
  ggtitle("Boxplot of Run Number by Year")

#Line and dotplot of number of events by date
plotJ <- ggplot(parkrunner, aes(x=Rundate, y=NumEvents)) +
  geom_point(colour="violetred") + 
  geom_line(colour="violetred") +
  xlab("Date") +
  ylab("Number of Events") +
  ggtitle("Number of Events by Date") +
  theme(axis.title = element_text(colour="violetred"),
        plot.title = element_text(colour="violetred"),
        axis.text = element_text(colour="grey36"))

print(plotA)
print(plotB)
print(plotC)
print(plotD)
print(plotE)
print(plotF)
print(plotG)
print(plotH)
print(plotI)
print(plotJ)

#End PDF creation
dev.off()
}

