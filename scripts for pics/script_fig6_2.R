#Author: Zaporozhtsev I.F.
#Created: May, 2020

library(readxl)
library(magick)
library(Rssa)


setwd("...")
t0b <- read_excel("./data_fig5.xlsx",sheet =3,col_names = TRUE,col_types=rep("numeric",each=6))
lens <- c(46,46,48)

t0b_ts_list <- vector(mode = "list", length = 3)
for (i in 1:3){
  t0b_ts_list[[i]] <- ts(t0b[1:lens[i],i+1],start=1970) 
}

res_main <- vector(mode="list",length = 5)
img1 <- image_graph(width = 1600, height = 1600, res = 200, pointsize = 1, clip = FALSE)
p1 <- plot(Rssa::wcor(Rssa::ssa(t0b_ts_list[[2]]), groups = 1:20),main=paste0("t050_",i))
print(p1)
dev.off()
image_write(img1, "Fugure5_single_wcor.png") 
