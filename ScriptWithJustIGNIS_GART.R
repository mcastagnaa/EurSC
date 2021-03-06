library(ggplot2)
library(reshape2)
library(plyr)
library(zoo)
library(PerformanceAnalytics)
library(scales)
library(quantmod)
library(Hmisc)

#Data management

rm(list =ls(all=TRUE))
RawData <- read.csv("RawData.csv", header=TRUE)
RawData$Date <- as.Date(RawData$Date, format="%d/%m/%Y")
n <-nrow(RawData)
head(RawData)
rownames(RawData) <- RawData$Date

#DOESN'T WORK periodReturn(as.ts(RawData[, 2:3]), period='monhtly')

# there must be something better than this!
RawData$IGNISret <- c(NA,RawData$IGNIS[2:n]/RawData$IGNIS[1:n-1]-1)
RawData$GARTPret <- c(NA,RawData$GARTPAN[2:n]/RawData$GARTPAN[1:n-1]-1)
RawData$JCSCEXPTret <- c(NA,RawData$JCSCEXPT[2:n]/RawData$JCSCEXPT[1:n-1]-1)

RawData$IGNISrr <- RawData$IGNISret - RawData$JCSCEXPTret
RawData$GARTPRrr <- RawData$GARTPret - RawData$JCSCEXPTret

head(RawData)

# relative returns data frame
RawData.rret <- subset(RawData, select=c(IGNISrr, 
                                         GARTPRrr))

rownames(RawData.rret) <- RawData$Date

# absolute returns data frame
RawData.r <- subset(RawData, select=c(IGNISret, 
                                      GARTPret,
                                      JCSCEXPTret))
RawData.r$Rf <- rep(0,nrow(RawData.r))
rownames(RawData.r) <- RawData$Date

# rolling standard deviation
RollingObs <- 54
RollSd <- data.frame(cbind(RawData$Date, 
                           rollapplyr(RawData[c("IGNISret", 
                                                "GARTPret",
                                                "JCSCEXPTret")], 
                                      RollingObs, sd, fill=NA)))
colnames(RollSd)[1] <- "Date"

RollSd$Date <- as.Date(RollSd$Date)
head(RollSd)

# charts
chart.Correlation(RawData.rret, histogram = TRUE)

chart.CumReturns(RawData.rret,
                 wealth.index = TRUE,
                 legend.loc = "topleft",
                 begin = "first",
                 main="Value of $1",
                 ylab = "'alpha' returns",
                 xlab = NULL, 
                 date.format = "%b/%y")


chart.CumReturns(RawData.r,
                 wealth.index = TRUE,
                 legend.loc = "topleft",
                 begin = "first",
                 main="Value of $1",
                 ylab = "absolute returns",
                 xlab = NULL, 
                 date.format = "%b/%y")


meltdf<-melt(RollSd,id="Date")

ggplot(na.omit(meltdf), 
      aes(x=Date, y=value, colour=variable, group=variable, size = variable, linetype = variable)) +
      geom_line() + 
      ylab(paste0("Rolling sd (", toString(RollingObs), "obs)")) +   
      scale_linetype_manual(values= c(rep("solid",7), "dotted")) +
      scale_size_manual(values= c(1, 1, 0.5)) + 
      theme(legend.position="bottom") + 
      scale_colour_brewer(palette="Set1")


# extra charts
charts.PerformanceSummary(RawData.rret)

chart.Boxplot(RawData.rret, sort.by = "variance")

chart.RiskReturnScatter(RawData.rret, 
                        add.boxplot = TRUE, 
                        sharpe.ratio = NULL)

chart.RollingPerformance(RawData.rret,
                         legend.loc = "topright",
                         begin = "first",
                         main="rolling 12-weeks performance",
                         ylab = "'alpha' returns",
                         xlab = NULL, 
                         date.format = "%b/%y")

# tables
t(table.CalendarReturns(RawData.rret))

result <- t(table.Stats(RawData.rret[, , drop = F]))
textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(rep(1,2),rep(3,14))),
         rmar = 0.8, cmar = 1.5,  max.cex=.9, halign = "center", valign = "top",
         row.valign="center", wrap.rownames=10, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
        title(main="Statistics for relative returns")

table.DownsideRisk(RawData.r[,1:2, drop=F], Rf = RawData.r[,3, drop=F])

table.Drawdowns(RawData.r[,1, drop=F])
