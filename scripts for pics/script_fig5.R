#Author: Zaporozhtsev I.F.
#Created: May, 2020

library(readxl)
library(ggplot2)
library(forecast)
library(TSA)
library(astsa)

setwd("...")

t050 <- read_excel("./data_fig5.xlsx",sheet =1,col_names = TRUE,col_types=rep("numeric",each=6))
t150200 <- read_excel("./data_fig5.xlsx",sheet =2,col_names = TRUE,col_types=rep("numeric",each=6))
t0b <- read_excel("./data_fig5.xlsx",sheet =3,col_names = TRUE,col_types=rep("numeric",each=6))
lens <- c(46,46,48,42,42)

t050_ts_list <- vector(mode = "list", length = 5)
t150200_ts_list <- vector(mode = "list", length = 5)
t0b_ts_list <- vector(mode = "list", length = 5)

for (i in 1:3){
  t050_ts_list[[i]] <- ts(t050[1:lens[i],i+1],start=1970) 
  t150200_ts_list[[i]] <- ts(t150200[1:lens[i],i+1],start=1970) 
  t0b_ts_list[[i]] <- ts(t0b[1:lens[i],i+1],start=1970) 
}

n <- 64

labels <- c("Coastal in Murm. Current","Main in Murm. Current","Main in Norw. Current","Main Current","Persey Current")

MyPeriodogram <- function(vals,n,ylabel,mainlabel){
  vals <-(vals - mean(vals))/sd(vals)
  fit <- arima(vals,order=c(1,0,0))  
  vals <- c(vals,rep(0,n-length(vals)))
  FF <-(2/n)*abs(fft(vals)^2)
  P <-FF[1:ceiling(0.000000001+n/2)]
  f <-(0:floor(0.000000001+n/2))/n
  plot(f, n/length(t050_ts_list[[1]])*P, ylab=ylabel,main=mainlabel,type="h",xlab ="frequency",lwd=5,cex.axis=8,cex.main=10,cex.lab=10,panel.first = grid(lwd=2,col = "black"))
  arspec <- ARMAspec(model=list(ar=c(fit$coef[1])),freq=f,plot=F)$spec
  lines(f,arspec,lty="dotted")  
  U = qchisq(.025,4) 
  L = qchisq(.975,4) 
  lines(f,2*arspec/L,col=2,lwd=2)
  lines(f,2*arspec/U,col=2,lwd=2)
  abline(v=0.33,col=2,lwd=5)
  abline(v=0.05,col=2,lwd=5)
}


png("Figure5_original.png",width = 3500, height = 3000)
par(mfrow=c(3, 3),plt=c(0.1,0.9,0.1,0.9),oma=c(6,2,4,2),mar = c(10,25,10,10),mgp=c(8,1,0))
for (i in 1:1){
  MyPeriodogram(t050_ts_list[[i]],n,labels[i],"0-50 m")
  MyPeriodogram(t150200_ts_list[[i]],n,"","150-200 m")
  MyPeriodogram(t0b_ts_list[[i]],n,"","0-bottom")
}
for (i in 2:3){
  MyPeriodogram(t050_ts_list[[i]],n,labels[i],"")
  MyPeriodogram(t150200_ts_list[[i]],n,"","")
  MyPeriodogram(t0b_ts_list[[i]],n,"","")
}
dev.off()


png("Figure5_diffs.png",width = 3500, height = 3000)
par(mfrow=c(3, 3),plt=c(0.1,0.9,0.1,0.9),oma=c(6,2,4,2),mar = c(10,25,10,10),mgp=c(8,1,0))
for (i in 1:1){
  MyPeriodogram(diff(t050_ts_list[[i]]),n,labels[i],"0-50 m")
  MyPeriodogram(diff(t150200_ts_list[[i]]),n,"","150-200 m")
  MyPeriodogram(diff(t0b_ts_list[[i]]),n,"","0-bottom")
}
for (i in 2:3){
  MyPeriodogram(diff(t050_ts_list[[i]]),n,labels[i],"")
  MyPeriodogram(diff(t150200_ts_list[[i]]),n,"","")
  MyPeriodogram(diff(t0b_ts_list[[i]]),n,"","")
}
dev.off()