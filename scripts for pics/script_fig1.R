#Author: Zaporozhtsev I.F.
#Created: May, 2020

library(dplyr)
library(tidyr)

setwd("...")

df <- readRDS("./data_fig1.rds")
filename <- "Salinity"
nrow(df)

df1 <- df %>% subset(lat < 69.6 + 4.5 + 0.25)
ddf1 <- aggregate(lat ~ month, df, length)
ddf2 <- aggregate(lat ~ month, df1, length)

dfres <- data.frame(month = ddf1$month,val1 = ddf1$lat,val2 = ddf2$lat)
dfres

dfres<- as.data.frame(t(dfres))
dfres <- dfres[2:3,]
colnames(dfres) <- 1:12
dfres <- as.matrix(dfres)
dfres

png(paste0("figure1_",filename,".png"),width = 1500, height = 1000,res=100)
par(mgp=c(5,1,0), mar=c(6, 12, 3, 2),oma=c(3, 3, 3, 3))
ze_barplot <- barplot(dfres, main=filename, beside=T , ,col=c("cornsilk4" , "grey") , ylim=c(0,1700) , ylab="Number of stations",
                      cex.main=3,cex = 2,cex.axis=1.8,cex.lab=3,xlab = "month")

legend(28, 1500, legend=c("All stations", "1-10 stations"),
       fill=c("cornsilk4" , "grey"), cex=1.8,
       title="Stations considered", text.font=4,bg='lightblue')

abline(h=c(500,1000,1500) , col="grey",lwd=2)
dev.off()
