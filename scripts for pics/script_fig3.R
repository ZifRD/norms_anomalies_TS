#Author: Zaporozhtsev I.F.
#Created: May, 2020

library(readxl)
library(pryr)
require(roperators)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)

setwd("...")

# Dserdseevsky dates
loadedxlsx <- read_excel("dates_fig3.xlsx",col_names = TRUE,col_types=c("numeric","date"))
loadedxlsx <- loadedxlsx %>% mutate(day = as.numeric(format(loadedxlsx$date,'%d')))
loadedxlsx <- loadedxlsx %>% mutate(mon = as.numeric(format(loadedxlsx$date,'%m')))
loadedxlsx <- loadedxlsx %>% mutate(total = paste0(year,'-',sprintf("%02d", mon),'-',sprintf("%02d", day)))
loadedxlsx <- loadedxlsx %>% mutate(left = as.character(ymd(total)-15))
loadedxlsx <- loadedxlsx %>% mutate(right = as.character(ymd(total)+15))

print(nrow(loadedxlsx))
print(ncol(loadedxlsx))

df <- readRDS("data_fig1.rds")
df <- df %>% subset(df$month >=7 & df$month <=10)
print(ncol(df))

df <- df %>% mutate(date = paste0(year,'-',sprintf("%02d", month),'-',sprintf("%02d", day)))
print(str(df))

mm <- matrix(0, 2019-1970+1, ymd("2020-11-01")-ymd("2020-07-01")+1)
indexm <- matrix(0,50,2)
for (i in 1:nrow(loadedxlsx)){
  indexm[i,1] <- ymd(loadedxlsx[i,6]) - ymd(paste0(as.character(loadedxlsx[i,1]),"-07-01"))+1
  indexm[i,2] <- ymd(loadedxlsx[i,7]) - ymd(paste0(as.character(loadedxlsx[i,1]),"-07-01"))+1
}

for (i in 1:nrow(df)){
  indexval <- ymd(df$date[i]) - ymd(paste0(as.character(df$year[i]),"-07-01"))+1
  mm[as.integer(df$year[i])-1970+1,indexval] = 100
}

rownames(mm) <- seq(1970,2019,by=1)

longData<-melt(mm)
longData<-longData[longData$value!=0,]

png("figure3.png",width = 4000, height = 4000,res=300)
gg <- ggplot(longData, aes(x = Var2, y = Var1,fill = value))

for (i in 1:nrow(mm)){
  gg <- gg + geom_segment(x=indexm[i,1], xend=indexm[i,2], y=1969+i, yend=1969+i, alpha=0.1,colour ="green",size=4)
}
gg <- gg +geom_point(size=3)+
  labs(x="Day of year", y="Year", title="") +
  theme_bw() + theme(axis.text.x=element_text(size=22),
                     axis.text.y=element_text(size=22),
                     axis.title=element_text(size=26,face="bold"),
                     panel.grid.major = element_line(size=1.5),
                     panel.grid.minor = element_blank())+
  scale_x_continuous(breaks =c(1,32,63,93), labels=c("July, 1st","August, 1st","September, 1st","October, 1st"))+
geom_vline(xintercept = 46,colour = "red",size = 2)+
geom_vline(xintercept = 76,colour = "red",size = 2)
print(gg)
dev.off()