#Author: Zaporozhtsev I.F.
#Created: May, 2020

library(readxl)
library(magick)
library(Rssa)

setwd("...")
t0b <- read_excel("./data_fig5.xlsx",sheet =3,col_names = TRUE,col_types=rep("numeric",each=6))

lens <- c(46,46,48)

t0b_ts_list <- vector(mode = "list", length = 3)

for (i in 1:5){
  t0b_ts_list[[i]] <- ts(t0b[1:lens[i],i+1],start=1970) 
}

data <- as.double(t0b_ts_list[[2]])
N <- length(data)
L <- floor(N/2)
s <- ssa(data,L=L,neig = min(L, N - L + 1))


g <- list(1,2:3,4:5,6:7)
fos <- fossa(s, nested.groups =g, gamma = 10,
             normalize = FALSE)

ios1 <- iossa(s, nested.groups = g,maxiter = 10)
ios1 <- iossa(ios1, nested.groups = g, maxiter = 10)
fo.rec <- reconstruct(fos, groups = g)
io.rec <- reconstruct(ios1, groups = g)
pr1 <- plot(fo.rec, plot.method = "xyplot",
            main = "Main in Murm Current, 150-200 m", xlab = "")
io.rec <- reconstruct(ios1, groups = ios1$iossa.groups)
pr2 <- plot(io.rec, plot.method = "xyplot",
            main = "Iterative O-SSA", xlab = "")
png("Figure6_reconstr.png",width = 1000, height = 1500,res=200)
plot(pr1, split = c(1, 1, 1, 2), more = TRUE)
plot(pr2, split = c(1, 2, 1, 2), more = FALSE)
dev.off()
