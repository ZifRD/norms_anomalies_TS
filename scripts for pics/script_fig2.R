#Author: Zaporozhtsev I.F.
#Created: May, 2020

library(pryr)
require(roperators)
library(dplyr)
library(ggplot2)
library(reshape2)

library(grid)
library(RColorBrewer)

make_gradient <- function(deg = 45, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}

colfunc <- colorRampPalette(c("white","cornsilk", "darkgoldenrod1","cornsilk"))

g <- make_gradient(
  deg = 90, n = 500, cols = colfunc(500))
)

df <- data.frame(num=numeric(0),st=numeric(0),typev=character(0))
df$typev <- as.character(df$typev)

i = 0
vallist <- vector("list",length = 48)
#1
vallist[[i%+=%1]] <- c(4,5,6,7,8,9,10)
vallist[[i%+=%1]] <- c(9,10,12,13)
vallist[[i%+=%1]] <- c(2:7)
vallist[[i%+=%1]] <- c(3,4,7:13)

#2
vallist[[i%+=%1]] <- c(1:10)
vallist[[i%+=%1]] <- c(2:9,12,13)
vallist[[i%+=%1]] <- c(2:10)
vallist[[i%+=%1]] <- c(2:13)
#3
vallist[[i%+=%1]] <- c(3:7,9,10)
vallist[[i%+=%1]] <- c(4,5)
vallist[[i%+=%1]] <- c(2:7,9,10)
vallist[[i%+=%1]] <- c(2:4)
#4
vallist[[i%+=%1]] <- c(4:10)
vallist[[i%+=%1]] <- c(3:11)
vallist[[i%+=%1]] <- c(3:8)
vallist[[i%+=%1]] <- c(3:7)
#5
vallist[[i%+=%1]] <- c(1:10)
vallist[[i%+=%1]] <- c(5:9,15:18)
vallist[[i%+=%1]] <- c(2:7,9,10)
vallist[[i%+=%1]] <- c(3:6,9,10)
#6
vallist[[i%+=%1]] <- c(1:10)
vallist[[i%+=%1]] <- c(1:9,15:17)
vallist[[i%+=%1]] <- c(1:10)
vallist[[i%+=%1]] <- c(2:7,11:17)

#7
vallist[[i%+=%1]] <- c(1:10)
vallist[[i%+=%1]] <- c(1:14,16:18)
vallist[[i%+=%1]] <- c(1:10)
vallist[[i%+=%1]] <- c(1:12)

#8
vallist[[i%+=%1]] <- c(1:10)
vallist[[i%+=%1]] <- c(1:18)
vallist[[i%+=%1]] <- c(1:10)
vallist[[i%+=%1]] <- c(1:14)

#9
vallist[[i%+=%1]] <- c(1:10)
vallist[[i%+=%1]] <- c(1:18)
vallist[[i%+=%1]] <- c()
vallist[[i%+=%1]] <- c(1:15)

#10
vallist[[i%+=%1]] <- c(1:10)
vallist[[i%+=%1]] <- c(1:4)
vallist[[i%+=%1]] <- c()
vallist[[i%+=%1]] <- c()

#11
vallist[[i%+=%1]] <- c(2:10)
vallist[[i%+=%1]] <- c(2:10)
vallist[[i%+=%1]] <- c(2:10)
vallist[[i%+=%1]] <- c(1:7,9,10,16:18)

#12
vallist[[i%+=%1]] <- c(2:10)
vallist[[i%+=%1]] <- c(3,4,6,7,9,10)
vallist[[i%+=%1]] <- c(2:9)
vallist[[i%+=%1]] <- c(3:7,9,10)

for(i in 1:48){
  typeval = "NULL"
  if(i %% 4 == 1) typeval = "OldT"
  if(i %% 4 == 2) typeval = "NewT"
  if(i %% 4 == 3) typeval = "OldS"
  if(i %% 4 == 0) typeval = "NewS"
  if(is.null(vallist[[i]])) next
  tempo <- vallist[[i]]
  print(i)
  print(typeval)
  print(tempo)
  for(j in 1:length(vallist[[i]])){
    df[nrow(df) + 1,] <- list(i,tempo[[j]],paste0(typeval))
  }
}

png("figure2.png",width = 3000, height = 2400,res=300)
gg <- ggplot(df, aes(x = num, y = st))

oldt_b <- c(4,6,1,6,3,6,9,4,6,1,6,1,6,1,1,1,1,2,2)
oldt_e <- c(5,10,5,10,5,7,10,5,10,5,10,5,10,10,10,10,10,10,10)
oldt_mon <- c(1,1,2,2,3,3,3,4,4,5,5,6,6,7,8,9,10,11,12)

newt_b <- c(9,12,2,7,12,4,3,6,5,7,15,1,15,1,16,1,15,1,15,1,2,5,7,9,3,6,9)
newt_e <- c(10,13,6,9,13,5,5,11,6,10,18,9,17,14,18,14,18,14,18,4,4,6,8,10,4,7,10)
newt_mon <- c(1,1,2,2,2,3,4,4,5,5,5,6,6,7,7,8,8,9,9,10,11,11,11,11,12,12,12)

olds_b <- c(2,2,8,2,9,3,2,9,1,3,8,1,1,8,2,2)
olds_e <- c(7,7,10,7,10,8,7,10,2,7,10,10,7,10,10,9)
olds_mon <- c(1,2,2,3,3,4,5,5,6,6,6,7,8,8,11,12)

news_b <- c(3,7,2,8,12,2,3,3,9,2,11,1,8,11,1,8,11,1,8,12,1,9,16,3,6,9)
news_e <- c(4,13,7,11,13,4,7,6,10,7,17,7,10,12,7,10,14,7,11,15,7,10,18,5,7,10)
news_mon <- c(1,1,2,2,2,3,4,5,5,6,6,7,7,7,8,8,8,9,9,9,11,11,11,12,12,12)

df1 <- data.frame(oldt_b = oldt_b,oldt_e = oldt_e,oldt_mon = oldt_mon)
df2 <- data.frame(newt_b = newt_b,newt_e = newt_e,newt_mon = newt_mon)
df3 <- data.frame(olds_b = olds_b,olds_e = olds_e,olds_mon = olds_mon)
df4 <- data.frame(news_b = news_b,news_e = news_e,news_mon = news_mon)

gg <- gg + annotation_custom(
  grob = g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)
gg <- gg + geom_hline(yintercept = 1:18,colour = "grey",size = 0.25)
gg <- gg + geom_hline(yintercept = c(3.5,7.5,10.5,14.5),colour = "black",size = 1)
gg <- gg + geom_vline(xintercept = seq(4.5,44.5,4.0),colour = "grey",size = 0.25)


gg <- gg + geom_segment(data = df1,aes(x=(oldt_mon-1)*4+1, xend=(oldt_mon-1)*4+1, y=oldt_b, yend=oldt_e), alpha=1,colour ="cyan2",size=2)
gg <- gg + geom_segment(data = df2,aes(x=(newt_mon-1)*4+2, xend=(newt_mon-1)*4+2, y=newt_b, yend=newt_e), alpha=1,colour ="darkblue",size=2)
gg <- gg + geom_segment(data = df3,aes(x=(olds_mon-1)*4+3, xend=(olds_mon-1)*4+3, y=olds_b, yend=olds_e), alpha=1,colour ="burlywood3",size=2)
gg <- gg + geom_segment(data = df4,aes(x=(news_mon-1)*4+4, xend=(news_mon-1)*4+4, y=news_b, yend=news_e), alpha=1,colour ="burlywood4",size=2)

gg <- gg +geom_point(data = df,aes(fill=typev, color=typev),shape=21, size=3)+
  scale_color_manual(values=c('darkorange','brown','darkorange', 'brown'))+
  scale_fill_manual(values=c('chartreuse','coral','chartreuse', 'coral'))+
  labs(x="month", y="station", title="") +
  theme_bw() + theme(axis.text.x=element_text(size=22),
                     axis.text.y=element_text(size=22),
                     axis.title=element_text(size=26,face="bold"),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())+
  scale_x_continuous(breaks =seq(2.5,47.0,4.0), labels="1":"12")+
  scale_y_continuous(breaks =1:18, labels="1":"18")+
  geom_vline(xintercept = 35,colour = "red",size = 1,linetype="longdash")+
  geom_vline(xintercept = 39,colour = "red",size = 1,linetype="longdash")+
  geom_vline(xintercept = 40,colour = "red",size = 1,linetype="longdash")
print(gg)
dev.off()