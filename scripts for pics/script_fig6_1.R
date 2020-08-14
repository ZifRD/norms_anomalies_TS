#Author: Zaporozhtsev I.F.
#Created: May, 2020

library(readxl)
library(magick)
library(Rssa)

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

res_main <- vector(mode="list",length = 5)
#plots <- NULL

#dev.new()
for (i in 1:3){
  res_main[[i]] <-local({
    i <- i
    mg1 <- image_graph(width = 800, height = 800, res = 200, pointsize = 1, clip = FALSE)
    p1 <- plot(Rssa::wcor(Rssa::ssa(t050_ts_list[[i]]), groups = 1:20),main=paste0("t050_",i))
    print(p1)
    dev.off()
    
    #plots <-c(plots,mg)
    mg2 <- image_graph(width = 800, height = 800, res = 200, pointsize = 1, clip = FALSE)
    p2 <- plot(Rssa::wcor(Rssa::ssa(t150200_ts_list[[i]]), groups = 1:20),main=paste0("t150200_",i))
    print(p2)
    dev.off()
    
    #plots <-c(plots,mg)
    mg3 <- image_graph(width = 800, height = 800, res = 200, pointsize = 1, clip = FALSE)
    p3 <- plot(Rssa::wcor(Rssa::ssa(t0b_ts_list[[i]]), groups = 1:20),main=paste0("t0b_",i))
    print(p3)
    dev.off()
    
    image_append(c(mg1,mg2,mg3))
  })
}  

img <- image_append(c(res_main[[1]],res_main[[2]],res_main[[3]],res_main[[4]],res_main[[5]]),stack=TRUE)
image_write(img, "Fugure5_panel_wcor.png") 
