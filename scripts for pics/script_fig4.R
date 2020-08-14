#Author: Zaporozhtsev I.F.
#Created: May, 2020

library(readxl)
library(reshape2)
library(ggplot2)
library(stringr)
library(metR)
library(RColorBrewer)

setwd("...")

# old norms (2000)
oldTnorms <- read_excel("./data_fig4.xlsx",sheet = 8,
                           range = cell_cols("B:K"),col_names = TRUE,
                           col_types=rep("numeric",each=10))

resultOld <- melt(as.matrix(oldTnorms[1:41,1:10]))
names(resultOld) <- c("dep", "st", "valls")
resultOld$dep <- (as.numeric(resultOld$dep)-1)*5
resultOld$st <- 69.5 + (as.integer(str_sub(resultOld$st, 2)) - 1)*0.5

breaks <- seq(1,10,0.5)
resultOld$valls[resultOld$valls > 10] <- 10
brlabels <-levels(unique(cut(resultOld$valls,breaks = breaks)))
resultOld$equalSpace <- cut(resultOld$valls, breaks = breaks)
 
png("Figure4.png",width = 2000, height = 2000,res=300)
ggplot(resultOld, aes(st, -dep, z = valls)) +
  theme_bw() +
  theme(axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))+
  labs(x="", y="Depth, m", title="")+
  scale_y_continuous(breaks =c(0,-50,-100,-150,-200), labels=c(0,50,100,150,200))+
  scale_x_continuous(breaks =seq(70,74,1), labels=paste0(seq(70,74,1),"°N"))+
  geom_contour_fill(breaks = seq(1,10,0.5)) +
  geom_contour2(breaks = seq(1,10,0.5),color = "black") +
  geom_text_contour(stroke = 0.3,size=6,rotate = FALSE)+
  scale_fill_divergent(breaks = seq(1,10,0.5),low = scales::muted("blue"),
                       mid = "white",
                       high = scales::muted("red"),limits=c(1,10),midpoint = 5.5)
dev.off()
