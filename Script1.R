library(ggplot2)
library(reshape2)
library(plyr)
library(zoo)

rm(list =ls(all=TRUE))
RawData <- read.csv("RawData.csv", header=TRUE)

head(RawData)
# apply(RawData[c("JP_LCEMD", "SH_LCEMD", "JP_EMD", "SH_EMD")], 2, function(x) sd(x, na.rm=TRUE)*sqrt(12))
# 
# RollingObs <- 20
# 
# RollSd <- data.frame(cbind(RawData$Date, 
#                            rollapplyr(RawData[c("JP_LCEMD", 
#                                                 "SH_LCEMD", 
#                                                 "JP_EMD", 
#                                                 "SH_EMD", 
#                                                 "B_LCEMD", 
#                                                 "B_EMD")], 
#                                       RollingObs, sd, fill=NA)))
# colnames(RollSd)[1] <- "Date"
# 
# head(RollSd)
# RollSd$Date <- as.Date(RollSd$Date)
# 
# meltdf<-melt(RollSd,id="Date")
# ggplot(na.omit(meltdf), 
#        aes(x=Date, y=value, colour=variable, group=variable, linetype = variable, size= variable)) +
#   geom_line() + 
#   ylab(paste0("Rolling sd (", toString(RollingObs), "obs)")) + 
#   scale_linetype_manual(values= c("solid","dashed","solid","dashed", "dotted", "dotted")) +
#   scale_color_manual(values= c("purple", "purple", "orange", "orange", "purple", "orange")) + 
#   scale_size_manual(values= c(1.1, 1.1, 1.1, 1.1, 1, 1)) + 
#   theme(legend.position="bottom")
# 
# 
# RawData$Date <- as.Date(RawData$Date, format="%d/%m/%Y")
# RawData$SH_EMDr <- RawData$SH_EMD - RawData$B_EMD
# RawData$SH_LEMDr <- RawData$SH_LCEMD - RawData$B_LCEMD
# RawData$JP_EMDr <- RawData$JP_EMD - RawData$B_EMD
# RawData$JP_LEMDr <- RawData$JP_LCEMD - RawData$B_LCEMD
# 
# head(RawData)
# 
# EMDset <- subset(RawData, !is.na(SH_EMD) | !is.na(JP_EMD), select=c(Date, SH_EMD, JP_EMD, B_EMD))
# LEMDset <- subset(RawData, !is.na(SH_LCEMD) | !is.na(JP_LCEMD), select=c(Date, SH_LCEMD, JP_LCEMD, B_LCEMD))
# EMDrr <- subset(RawData, !is.na(SH_EMD) | !is.na(JP_EMD), select=c(Date, SH_EMDr, JP_EMDr))
# LEMDrr <- subset(RawData, !is.na(SH_LCEMD) | !is.na(JP_LCEMD), select=c(Date, SH_LEMDr, JP_LEMDr))
# 
# W_JP_LCEMD <- data.frame(W_JP_LCEMD=RawData$B_LCEMD[!is.na(RawData$JP_LCEMD)])
# W_SH_LCEMD <- data.frame(W_SH_LCEMD=RawData$B_LCEMD[!is.na(RawData$SH_LCEMD)])
# W_JP_EMD <- data.frame(W_JP_EMD=RawData$B_EMD[!is.na(RawData$JP_EMD)])
# W_SH_EMD <- data.frame(W_SH_EMD=RawData$B_EMD[!is.na(RawData$SH_EMD)])
# 
# WORLDS <- do.call(rbind.fill, list(W_JP_LCEMD, W_SH_LCEMD, W_JP_EMD, W_SH_EMD))
# apply(WORLDS, 2, function(x) sd(x, na.rm=TRUE)*sqrt(12))
# rm(list =ls(pattern="W_"))
# 
# meltdf<-melt(EMDset,id="Date")
# ggplot(meltdf, 
#         aes(x=Date, y=value, colour=variable, group=variable)) +
#         geom_line() + 
#         ylab("Absolute Returns") + 
#         theme(legend.position="bottom")
# 
# 
# meltdf<-melt(LEMDset,id="Date")
# ggplot(meltdf, 
#       aes(x=Date, y=value, colour=variable, group=variable)) +
#       geom_line() + 
#       ylab("Absolute Returns") + 
#       theme(legend.position="bottom")
# 
# meltdf<-melt(EMDrr,id="Date")
# ggplot(meltdf, 
#        aes(x=Date, y=value, colour=variable, group=variable)) +
#   geom_line() + 
#   ylab("Relative Returns") + 
#   theme(legend.position="bottom")
# 
# meltdf<-melt(LEMDrr,id="Date")
# ggplot(meltdf, 
#        aes(x=Date, y=value, colour=variable, group=variable)) +
#   geom_line() + 
#   ylab("Relative Returns") + 
#   theme(legend.position="bottom")
# 
# LEMDrr <- subset(RawData, !is.na(SH_LCEMD) & !is.na(JP_LCEMD), select=c(Date, SH_LEMDr, JP_LEMDr))
# meltdf<-melt(LEMDrr,id="Date")
# ggplot(meltdf, 
#        aes(x=Date, y=value, colour=variable, group=variable)) +
#   geom_line() + 
#   ylab("Relative Returns") + 
#   theme(legend.position="bottom")
# 
# head(EMDrr)
# ks.test(EMDrr$SH_EMDr, EMDrr$JP_EMDr, alternative="two.sided", exact=NULL)
# 
# LEMDrr <- subset(RawData, !is.na(SH_LCEMD) | !is.na(JP_LCEMD), select=c(Date, SH_LEMDr, JP_LEMDr))
# head(LEMDrr)
# ks.test(LEMDrr$SH_LEMDr, LEMDrr$JP_LEMDr, alternative="two.sided", exact=NULL)
# 
# SH <- na.omit(EMDrr$SH_EMDr)
# JP <- na.omit(EMDrr$JP_EMDr)
# 
# plot(ecdf(SH), 
#      xlim=range(c(SH, JP)))
# plot(ecdf(JP), add=TRUE, lty="dashed")
# 
# SH <- SH[SH >-5]
# #get rid of 10/09 observation
# 
# #ddply(RawData, c("JP_LCEMD", "JP_EMD", "SH_LCEMD", "SH_EMD"), summarize, sd=sd(value), sda=sd(value)*sqrt(12))
# 
